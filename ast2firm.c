#include <config.h>

#define _GNU_SOURCE

#include <assert.h>
#include <string.h>
#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "ast_t.h"
#include "type_t.h"
#include "semantic.h"
#include "mangle_type.h"
#include "adt/array.h"
#include "adt/obst.h"
#include "adt/strset.h"
#include "adt/error.h"
#include "adt/xmalloc.h"
#include <libfirm/adt/pdeq.h>

//#define DEBUG_TYPEVAR_BINDING

static const variable_declaration_t **value_numbers = NULL;
static label_declaration_t           *labels        = NULL;

typedef struct instantiate_method_t  instantiate_method_t;

static ir_type *byte_ir_type  = NULL;
static ir_type *void_ptr_type = NULL;

struct instantiate_method_t {
	method_t             *method;
	symbol_t             *symbol;
	type_argument_t      *type_arguments;
	ir_entity            *entity;
};

typedef struct type2firm_env_t type2firm_env_t;
struct type2firm_env_t {
	int can_cache;       /* nonzero if type can safely be cached because
	                        no typevariables are in the hierarchy */
};

typedef struct typevar_binding_t typevar_binding_t;
struct typevar_binding_t {
	type_variable_t *type_variable;
	type_t          *old_current_type;
};

static struct obstack obst;
static strset_t              instantiated_methods;
static pdeq                 *instantiate_methods  = NULL;
static typevar_binding_t    *typevar_binding_stack = NULL;

static
ir_type *_get_ir_type(type2firm_env_t *env, type_t *type);
static
ir_type *get_ir_type(type_t *type);

static
ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos)
{
	fprintf(stderr, "Warning: variable '%s' might be used uninitialized\n",
			value_numbers[pos]->symbol->string);
	return new_r_Unknown(irg, mode);
}

void initialize_firm(void)
{
	const backend_params *be_params;
	firm_parameter_t params;
	memset(&params, 0, sizeof(params));

#if 0
	/* read firm options */
	firm_init_options("mlang", 0, NULL);
#endif

	params.size = sizeof(params);
	params.enable_statistics = 0;
	params.initialize_local_func = uninitialized_local_var;
	params.cc_mask = 0;
	params.builtin_dbg = NULL;

	/* initialize backend */
	be_params = be_init();
	params.arch_op_settings = be_params->arch_op_settings;

	/* intialize firm itself */
	init_firm(&params);

	atomic_type_t byte_type;
	memset(&byte_type, 0, sizeof(byte_type));
	byte_type.type.type = TYPE_ATOMIC;
	byte_type.atype     = ATOMIC_TYPE_BYTE;

	byte_ir_type = get_ir_type((type_t*) &byte_type);

	ir_type *ir_type_void = get_ir_type(type_void);
	void_ptr_type         = new_type_pointer(new_id_from_str("void_ptr"),
	                                         ir_type_void, mode_P_data);
}

void exit_firm(void)
{
}

static
ident *unique_id(const char *tag)
{
	static unsigned id = 0;
	char            buf[256];

	snprintf(buf, sizeof(buf), "%s%d", tag, id);
	id++;
	return new_id_from_str(buf);
}

static
ir_mode *get_atomic_mode(const atomic_type_t* atomic_type)
{
	switch(atomic_type->atype) {
	case ATOMIC_TYPE_BYTE:
		return mode_Bs;
	case ATOMIC_TYPE_UBYTE:
		return mode_Bu;
	case ATOMIC_TYPE_SHORT:
		return mode_Hs;
	case ATOMIC_TYPE_USHORT:
		return mode_Hu;
	case ATOMIC_TYPE_INT:
		return mode_Is;
	case ATOMIC_TYPE_UINT:
		return mode_Iu;
	case ATOMIC_TYPE_LONG:
		return mode_Ls;
	case ATOMIC_TYPE_ULONG:
		return mode_Lu;
	case ATOMIC_TYPE_LONGLONG:
		return mode_LLs;
	case ATOMIC_TYPE_ULONGLONG:
		return mode_LLu;
	case ATOMIC_TYPE_FLOAT:
		return mode_F;
	case ATOMIC_TYPE_DOUBLE:
		return mode_D;
	case ATOMIC_TYPE_BOOL:
		return mode_Is;
	default:
		panic("Encountered unknown atomic type");
	}
}



static
unsigned get_type_size(type_t *type);

static
unsigned get_atomic_type_size(const atomic_type_t *type)
{
	switch(type->atype) {
	case ATOMIC_TYPE_UBYTE:
	case ATOMIC_TYPE_BYTE:
		return 1;

	case ATOMIC_TYPE_BOOL:
	case ATOMIC_TYPE_INT:
	case ATOMIC_TYPE_UINT:
	case ATOMIC_TYPE_LONG:
	case ATOMIC_TYPE_ULONG:
	case ATOMIC_TYPE_FLOAT:
		return 4;

	case ATOMIC_TYPE_SHORT:
	case ATOMIC_TYPE_USHORT:
		return 2;

	case ATOMIC_TYPE_LONGLONG:
	case ATOMIC_TYPE_ULONGLONG:
	case ATOMIC_TYPE_DOUBLE:
		return 8;

	case ATOMIC_TYPE_INVALID:
		break;
	}
	panic("Trying to determine size of invalid atomic type");
}

static
unsigned get_compound_type_size(compound_type_t *type)
{
	ir_type *irtype = get_ir_type(&type->type);
	return get_type_size_bytes(irtype);
}

static
unsigned get_type_reference_type_var_size(const type_reference_t *type)
{
	type_variable_t *type_variable = type->r.type_variable;


	if(type_variable->current_type == NULL) {
		panic("taking size of unbound type variable");
		return 0;
	}
	return get_type_size(type_variable->current_type);
}

static
unsigned get_array_type_size(array_type_t *type)
{
	ir_type *irtype = get_ir_type(&type->type);
	return get_type_size_bytes(irtype);
}

static
unsigned get_type_size(type_t *type)
{
	switch(type->type) {
	case TYPE_VOID:
		return 0;
	case TYPE_ATOMIC:
		return get_atomic_type_size((const atomic_type_t*) type);
	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		return get_compound_type_size((compound_type_t*) type);
	case TYPE_METHOD:
		/* just a pointer to the method */
		return get_mode_size_bytes(mode_P_code);
	case TYPE_POINTER:
		return get_mode_size_bytes(mode_P_data);
	case TYPE_ARRAY:
		return get_array_type_size((array_type_t*) type);
	case TYPE_REFERENCE:
		panic("Type reference not resolved");
		break;
	case TYPE_REFERENCE_TYPE_VARIABLE:
		return get_type_reference_type_var_size((type_reference_t*) type);
	case TYPE_INVALID:
		break;
	}
	panic("Trying to determine size of invalid type");
}

static
int count_parameters(const method_type_t *method_type)
{
	int count = 0;

	method_parameter_type_t *param_type = method_type->parameter_types;
	while(param_type != NULL) {
		param_type = param_type->next;
		count++;
	}

	return count;
}




static
ir_type *get_atomic_type(type2firm_env_t *env, const atomic_type_t *type)
{
	(void) env;
	ir_mode *mode   = get_atomic_mode(type);
	ident   *id     = get_mode_ident(mode);
	ir_type *irtype = new_type_primitive(id, mode);

	return irtype;
}

static
ir_type *get_method_type(type2firm_env_t *env, const method_type_t *method_type)
{
	type_t  *result_type  = method_type->result_type;

	ident   *id           = unique_id("methodtype");
	int      n_parameters = count_parameters(method_type);
	int      n_results    = result_type->type == TYPE_VOID ? 0 : 1;
	ir_type *irtype       = new_type_method(id, n_parameters, n_results);

	if(result_type->type != TYPE_VOID) {
		ir_type *restype = _get_ir_type(env, result_type);
		set_method_res_type(irtype, 0, restype);
	}

	method_parameter_type_t *param_type = method_type->parameter_types;
	int n = 0;
	while(param_type != NULL) {
		ir_type *p_irtype = _get_ir_type(env, param_type->type);
		set_method_param_type(irtype, n, p_irtype);

		param_type = param_type->next;
		n++;
	}

	if(method_type->variable_arguments) {
		set_method_variadicity(irtype, variadicity_variadic);
	}

	return irtype;
}

static
ir_type *get_pointer_type(type2firm_env_t *env, pointer_type_t *type)
{
	type_t  *points_to = type->points_to;
	ir_type *ir_points_to;
	/* Avoid endless recursion if the points_to type contains this poiner type
	 * again (might be a struct). We therefore first create a void* pointer
	 * and then set the real points_to type
	 */
	ir_type *ir_type_void = get_ir_type(type_void);
	ir_type *ir_type      = new_type_pointer(unique_id("pointer"),
                                             ir_type_void, mode_P_data);
	type->type.firm_type  = ir_type;

	ir_points_to = _get_ir_type(env, points_to);
	set_pointer_points_to_type(ir_type, ir_points_to);

	return ir_type;
}

static
ir_type *get_array_type(type2firm_env_t *env, array_type_t *type)
{
	type_t  *element_type    = type->element_type;
	ir_type *ir_element_type = _get_ir_type(env, element_type);

	ir_type *ir_type = new_type_array(unique_id("array"), 1, ir_element_type);
	set_array_bounds_int(ir_type, 0, 0, type->size);

	size_t elemsize = get_type_size_bytes(ir_element_type);
	int align = get_type_alignment_bytes(ir_element_type);
	if(elemsize % align > 0) {
		elemsize += align - (elemsize % align);
	}
	set_type_size_bytes(ir_type, type->size * elemsize);
	set_type_alignment_bytes(ir_type, align);
	set_type_state(ir_type, layout_fixed);

	return ir_type;
}

#define INVALID_TYPE ((ir_type_ptr)-1)

static
ir_type *get_struct_type(type2firm_env_t *env, compound_type_t *type)
{
	ir_type *ir_type = new_type_struct(unique_id(type->symbol->string));
	type->type.firm_type = ir_type;

	int align_all = 1;
	int offset    = 0;
	compound_entry_t *entry = type->entries;
	while(entry != NULL) {
		ident       *ident         = new_id_from_str(entry->symbol->string);
		ir_type_ptr  entry_ir_type = _get_ir_type(env, entry->type);

		int entry_size      = get_type_size_bytes(entry_ir_type);
		int entry_alignment = get_type_alignment_bytes(entry_ir_type);
		int misalign = offset % entry_alignment;
		offset += misalign;

		ir_entity *entity = new_entity(ir_type, ident, entry_ir_type);
		set_entity_offset(entity, offset);
		add_struct_member(ir_type, entity);
		entry->entity = entity;

		offset += entry_size;
		if(entry_alignment > align_all) {
			if(entry_alignment % align_all != 0) {
				panic("Uneven alignments not supported yet");
			}
			align_all = entry_alignment;
		}
		entry = entry->next;
	}

	int misalign = offset % align_all;
	offset += misalign;
	set_type_alignment_bytes(ir_type, align_all);
	set_type_size_bytes(ir_type, offset);
	set_type_state(ir_type, layout_fixed);

	return ir_type;
}

static
ir_type *get_union_type(type2firm_env_t *env, compound_type_t *type)
{
	symbol_t *symbol  = type->symbol;
	ident    *id      = unique_id(symbol->string);
	ir_type  *ir_type = new_type_union(id);

	type->type.firm_type = ir_type;

	int align_all = 1;
	int size      = 0;
	compound_entry_t *entry = type->entries;
	while(entry != NULL) {
		ident       *ident         = new_id_from_str(entry->symbol->string);
		ir_type_ptr  entry_ir_type = _get_ir_type(env, entry->type);

		int entry_size      = get_type_size_bytes(entry_ir_type);
		int entry_alignment = get_type_alignment_bytes(entry_ir_type);

		ir_entity *entity = new_entity(ir_type, ident, entry_ir_type);
		add_union_member(ir_type, entity);
		set_entity_offset(entity, 0);
		entry->entity = entity;

		if(entry_size > size) {
			size = entry_size;
		}
		if(entry_alignment > align_all) {
			if(entry_alignment % align_all != 0) {
				panic("Uneven alignments not supported yet");
			}
			align_all = entry_alignment;
		}

		entry = entry->next;
	}

	set_type_alignment_bytes(ir_type, align_all);
	set_type_size_bytes(ir_type, size);
	set_type_state(ir_type, layout_fixed);

	return ir_type;
}



static
ir_type *get_type_for_type_variable(type2firm_env_t *env,
                                    type_reference_t *ref)
{
	assert(ref->type.type == TYPE_REFERENCE_TYPE_VARIABLE);
	type_variable_t *type_variable = ref->r.type_variable;
	type_t          *current_type  = type_variable->current_type;

	if(current_type == NULL) {
		fprintf(stderr, "Panic: trying to transform unbound type variable "
		        "'%s'\n", type_variable->declaration.symbol->string);
		abort();
	}
	ir_type *ir_type = _get_ir_type(env, current_type);
	env->can_cache   = 0;

	return ir_type;
}

static
ir_type *_get_ir_type(type2firm_env_t *env, type_t *type)
{
	assert(type != NULL);

	if(type->firm_type != NULL) {
		assert(type->firm_type != INVALID_TYPE);
		return type->firm_type;
	}

	ir_type *firm_type = NULL;
	switch(type->type) {
	case TYPE_ATOMIC:
		firm_type = get_atomic_type(env, (atomic_type_t*) type);
		break;
	case TYPE_METHOD:
		firm_type = get_method_type(env, (method_type_t*) type);
		break;
	case TYPE_POINTER:
		firm_type = get_pointer_type(env, (pointer_type_t*) type);
		break;
	case TYPE_ARRAY:
		firm_type = get_array_type(env, (array_type_t*) type);
		break;
	case TYPE_VOID:
		/* there is no mode_VOID in firm, use mode_C */
		firm_type = new_type_primitive(new_id_from_str("void"), mode_C);
		break;
	case TYPE_COMPOUND_STRUCT:
		firm_type = get_struct_type(env, (compound_type_t*) type);
		break;
	case TYPE_COMPOUND_UNION:
		firm_type = get_union_type(env, (compound_type_t*) type);
		break;
	case TYPE_REFERENCE_TYPE_VARIABLE:
		firm_type = get_type_for_type_variable(env, (type_reference_t*) type);
		break;
	default:
		panic("unknown type");
	}

	if(env->can_cache) {
		type->firm_type = firm_type;
	}
	return firm_type;

}

static
ir_type *get_ir_type(type_t *type)
{
	type2firm_env_t env;
	env.can_cache = 1;

	return _get_ir_type(&env, type);
}

static inline
ir_mode *get_ir_mode(type_t *type)
{
	ir_type *irtype = get_ir_type(type);
	ir_mode *mode   = get_type_mode(irtype);
	assert(mode != NULL);
	return mode;
}

static
instantiate_method_t *queue_method_instantiation(method_t *method,
                                                 symbol_t *symbol)
{
	instantiate_method_t *instantiate
		= obstack_alloc(&obst, sizeof(instantiate[0]));
	memset(instantiate, 0, sizeof(instantiate[0]));

	instantiate->method = method;
	instantiate->symbol = symbol;
	pdeq_putr(instantiate_methods, instantiate);

	return instantiate;
}

static
int is_polymorphic_method(const method_t *method)
{
	return method->type_parameters != NULL;
}

static
int typevar_binding_stack_top()
{
	return ARR_LEN(typevar_binding_stack);
}

static
void push_type_variable_bindings(type_variable_t *type_parameters,
                                 type_argument_t *type_arguments)
{
	type_variable_t *type_var;
	type_argument_t *argument;

	if(type_parameters == NULL || type_arguments == NULL)
		return;

	/* we have to take care that all rebinding happens atomically, so we first
	 * create the structures on the binding stack and misuse the
	 * old_current_type value to temporarily save the new! current_type.
	 * We can then walk the list and set the new types */
	type_var = type_parameters;
	argument = type_arguments;
	int old_top = typevar_binding_stack_top();
	int top = ARR_LEN(typevar_binding_stack) + 1;
	while(type_var != NULL) {
		type_t *type = argument->type;
		while(type->type == TYPE_REFERENCE_TYPE_VARIABLE) {
			type_reference_t *ref = (type_reference_t*) type;
			type_variable_t  *var = ref->r.type_variable;

			if(var->current_type == NULL) {
				fprintf(stderr, "Type variable '%s' not bound\n",
				        var->declaration.symbol->string);
				abort();
			}
			type = var->current_type;
		}

		top = ARR_LEN(typevar_binding_stack) + 1;
		ARR_RESIZE(typevar_binding_stack, top);

		typevar_binding_t *binding = & typevar_binding_stack[top-1];
		binding->type_variable     = type_var;
		binding->old_current_type  = type;

		type_var = type_var->next;
		argument = argument->next;
	}
	assert(type_var == NULL && argument == NULL);

	for(int i = old_top+1; i <= top; ++i) {
		typevar_binding_t *binding       = & typevar_binding_stack[i-1];
		type_variable_t   *type_variable = binding->type_variable;
		type_t            *new_type      = binding->old_current_type;

		binding->old_current_type   = type_variable->current_type;
		type_variable->current_type = new_type;

#ifdef DEBUG_TYPEVAR_BINDING
		fprintf(stderr, "binding '%s'(%p) to ", type_variable->symbol->string,
		        type_variable);
		print_type(stderr, type_variable->current_type);
		fprintf(stderr, "\n");
#endif
	}
}

static
void pop_type_variable_bindings(int new_top)
{
	int top = ARR_LEN(typevar_binding_stack) - 1;
	for(int i = top; i >= new_top; --i) {
		typevar_binding_t *binding       = & typevar_binding_stack[i];
		type_variable_t   *type_variable = binding->type_variable;
		type_variable->current_type      = binding->old_current_type;

#ifdef DEBUG_TYPEVAR_BINDING
		fprintf(stderr, "reset binding of '%s'(%p) to ",
		        type_variable->symbol->string, type_variable);
		print_type(stderr, binding->old_current_type);
		fprintf(stderr, "\n");
#endif
	}

	ARR_SHRINKLEN(typevar_binding_stack, new_top);
}

static
ir_entity* get_typeclass_method_instance_entity(
		typeclass_method_instance_t *method_instance)
{
	method_t *method = & method_instance->method;
	if(method->entity != NULL)
		return method->entity;

	method_type_t *method_type = method->type;

	typeclass_method_t *typeclass_method = method_instance->typeclass_method;
	typeclass_t        *typeclass        = typeclass_method->typeclass;
	const char         *string           
		= typeclass->declaration.symbol->string;
	size_t              len              = strlen(string);
	obstack_printf(&obst, "_tcv%zu%s", len, string);

	string = typeclass_method->declaration.symbol->string;
	len    = strlen(string);
	obstack_printf(&obst, "%zu%s", len, string);

	typeclass_instance_t *instance = method_instance->typeclass_instance;
	type_argument_t      *argument = instance->type_arguments;
	while(argument != NULL) {
		mangle_type(&obst, argument->type);
		argument = argument->next;
	}
	obstack_1grow(&obst, 0);

	char *str = obstack_finish(&obst);

	ident *id = new_id_from_str(str);
	obstack_free(&obst, str);

	/* create the entity */
	ir_type *global_type    = get_glob_type();
	ir_type *ir_method_type = get_ir_type((type_t*) method_type);

	ir_entity *entity       = new_entity(global_type, id, ir_method_type);
	set_entity_ld_ident(entity, id);
	set_entity_visibility(entity, visibility_external_visible);

	method->entity = entity;
	return entity;
}

static
ir_entity* get_method_entity(method_t *method, symbol_t *symbol)
{
	if(method->entity != NULL) {
		return method->entity;
	}

	method_type_t *method_type    = method->type;
	int            is_polymorphic = is_polymorphic_method(method);

	ident *id;
	if(!is_polymorphic) {
		id = new_id_from_str(symbol->string);
	} else {
		const char *string = symbol->string;
		size_t      len    = strlen(string);
		obstack_printf(&obst, "_v%zu%s", len, string);

		type_variable_t *type_variable = method->type_parameters;
		while(type_variable != NULL) {
			mangle_type(&obst, type_variable->current_type);
			
			type_variable = type_variable->next;
		}
		obstack_1grow(&obst, 0);

		char *str = obstack_finish(&obst);

		id = new_id_from_str(str);
		obstack_free(&obst, str);
	}

	/* create the entity */
	ir_type *global_type    = get_glob_type();
	ir_type *ir_method_type = get_ir_type((type_t*) method_type);

	ir_entity *entity       = new_entity(global_type, id, ir_method_type);
	set_entity_ld_ident(entity, id);
	if(method->is_extern) {
		set_entity_visibility(entity, visibility_external_allocated);
	} else if(!is_polymorphic) {
		set_entity_visibility(entity, visibility_external_visible);
	} else {
		/* TODO find out why visibility_local doesn't work */
		set_entity_visibility(entity, visibility_external_visible);
	}

	if(!is_polymorphic) {
		method->entity = entity;
	}
	return entity;
}

static
ir_node *load_from_expression_addr(type_t *type, ir_node *addr);

static
ir_node *expression_to_firm(const expression_t *expression);

static
ir_node *int_const_to_firm(const int_const_t *cnst)
{
	ir_mode *mode = get_ir_mode(cnst->expression.datatype);
	tarval *tv    = new_tarval_from_long(cnst->value, mode);

	return new_Const(mode, tv);
}

static
ir_node *string_const_to_firm(const string_const_t* cnst)
{
	ir_type   *global_type = get_glob_type();
	ir_type   *type        = new_type_array(unique_id("bytearray"), 1,
	                                        byte_ir_type);

	ir_entity *ent = new_entity(global_type, unique_id("str"), type);
	set_entity_variability(ent, variability_constant);

	ir_type    *elem_type = byte_ir_type;
	ir_mode    *mode      = get_type_mode(elem_type);

	const char *string = cnst->value;
	size_t      slen   = strlen(string) + 1;

	set_array_lower_bound_int(type, 0, 0);
	set_array_upper_bound_int(type, 0, slen);
	set_type_size_bytes(type, slen);
	set_type_state(type, layout_fixed);

	tarval    **tvs = xmalloc(slen * sizeof(tvs[0]));
	for(size_t i = 0; i < slen; ++i) {
		tvs[i] = new_tarval_from_long(string[i], mode);
	}

	set_array_entity_values(ent, tvs, slen);
	free(tvs);

	return new_SymConst((union symconst_symbol) ent, symconst_addr_ent);
}

static
ir_node *null_pointer_to_firm(void)
{
	ir_mode *mode = get_type_mode(void_ptr_type);
	tarval  *tv   = get_tarval_null(mode);

	return new_Const(mode, tv);
}

static
ir_node *variable_reference_to_firm(const variable_declaration_t *variable)
{
	ir_mode *mode = get_ir_mode(variable->type);

	assert(variable->value_number < get_irg_n_locs(current_ir_graph));
	value_numbers[variable->value_number] = variable;

	return get_value(variable->value_number, mode);
}

static
ir_node *select_expression_addr(const select_expression_t *select)
{
	expression_t *compound_ptr      = select->compound;
	/* make sure the firm type for the struct is constructed */
	get_ir_type(compound_ptr->datatype);
	ir_node      *compound_ptr_node = expression_to_firm(compound_ptr);
	ir_node      *nomem             = new_NoMem();
	ir_entity    *entity            = select->compound_entry->entity;
	ir_node      *addr              = new_simpleSel(nomem, compound_ptr_node,
	                                                entity);

	return addr;
}

static
ir_node *array_access_expression_addr(const array_access_expression_t* access)
{
	expression_t *array_ref  = access->array_ref;
	expression_t *index      = access->index;
	ir_node      *base_addr  = expression_to_firm(array_ref);
	ir_node      *index_node = expression_to_firm(index);

	int           elem_size       = get_type_size(access->expression.datatype);
	tarval       *elem_size_tv    = new_tarval_from_long(elem_size, mode_Is);
	ir_node      *elem_size_const = new_Const(mode_Is, elem_size_tv);

	ir_node      *mul = new_Mul(index_node, elem_size_const, mode_Is);
	ir_node      *add = new_Add(base_addr, mul, mode_P_data);
	return add;
}

static
void create_global_variable_entity(global_variable_t *variable)
{
	if(variable->entity != NULL)
		return;

	ir_type *globtype = get_glob_type();
	ident   *ident    = new_id_from_str(variable->declaration.symbol->string);
	type_t  *type     = variable->type;
	ir_type *irtype   = get_ir_type(type);

	ir_entity *entity = new_entity(globtype, ident, irtype);
	set_entity_ld_ident(entity, ident);
	set_entity_variability(entity, variability_uninitialized);
	if(variable->is_extern) {
		set_entity_visibility(entity, visibility_external_allocated);
	} else {
		set_entity_visibility(entity, visibility_external_visible);
	}

	variable->entity = entity;
}

static
ir_node *global_variable_addr(global_variable_t *variable)
{
	create_global_variable_entity(variable);

	ir_node *symconst = new_SymConst((union symconst_symbol) variable->entity,
	                                 symconst_addr_ent);
	return symconst;
}

static
ir_node *global_variable_to_firm(global_variable_t *variable)
{
	ir_node *addr = global_variable_addr(variable);
	type_t  *type = variable->type;

	if(type->type == TYPE_COMPOUND_STRUCT 
			|| type->type == TYPE_COMPOUND_UNION
			|| type->type == TYPE_ARRAY) {
		return addr;
	}

	return load_from_expression_addr(type, addr);
}

static
ir_node *constant_reference_to_firm(const constant_t *constant)
{
	return expression_to_firm(constant->expression);
}

static
ir_node *reference_expression_addr(const reference_expression_t *reference)
{
	declaration_t *declaration = reference->declaration;

	switch(declaration->type) {
	case DECLARATION_GLOBAL_VARIABLE:
		return global_variable_addr((global_variable_t*) declaration);

	case DECLARATION_INVALID:
	case DECLARATION_METHOD:
	case DECLARATION_METHOD_PARAMETER:
	case DECLARATION_VARIABLE:
	case DECLARATION_CONSTANT:
	case DECLARATION_LABEL:
	case DECLARATION_TYPEALIAS:
	case DECLARATION_TYPECLASS:
	case DECLARATION_TYPECLASS_METHOD:
	case DECLARATION_TYPE_VARIABLE:
	case DECLARATION_LAST:
		panic("internal error: trying to create address nodes for non-lvalue");
	}
	panic("Unknown declaration found in reference expression");
}

static
ir_node *expression_addr(const expression_t *expression)
{
	const unary_expression_t  *unexpr;
	const select_expression_t *select;

	switch(expression->type) {
	case EXPR_SELECT:
		select = (const select_expression_t*) expression;
		return select_expression_addr(select);
	case EXPR_ARRAY_ACCESS:
		return array_access_expression_addr(
				(const array_access_expression_t*) expression);
	case EXPR_REFERENCE:
		return reference_expression_addr(
				(const reference_expression_t*) expression);
	case EXPR_UNARY:
		unexpr = (const unary_expression_t*) expression;
		if(unexpr->type == UNEXPR_DEREFERENCE) {
			return expression_to_firm(unexpr->value);
		}
		break;
	default:
		break;
	}
	panic("trying to get address from non lvalue construct");
}

static
ir_node *assign_expression_to_firm(const binary_expression_t *assign)
{
	const expression_t *left  = assign->left;
	const expression_t *right = assign->right;
	ir_node            *value = expression_to_firm(right);

	if(left->type == EXPR_REFERENCE) {
		const reference_expression_t *ref 
			= (const reference_expression_t*) left;
		declaration_t *declaration = ref->declaration;

		if(declaration->type == DECLARATION_VARIABLE) {
			variable_declaration_t *variable 
				= (variable_declaration_t*) declaration;

			value_numbers[variable->value_number] = variable;
			set_value(variable->value_number, value);
			return value;
		}
	}

	ir_node *addr       = expression_addr(left);
	ir_node *store      = get_store();
	ir_node *store_node = new_Store(store, addr, value);
	ir_node *mem        = new_Proj(store_node, mode_M, pn_Store_M);
	set_store(mem);

	return value;
}

static
ir_op *binexpr_type_to_op(binary_expression_type_t type)
{
	switch(type) {
	case BINEXPR_ADD:
		return op_Add;
	case BINEXPR_SUB:
		return op_Sub;
	case BINEXPR_MUL:
		return op_Mul;
	case BINEXPR_AND:
		return op_And;
	case BINEXPR_OR:
		return op_Or;
	case BINEXPR_XOR:
		return op_Eor;
	case BINEXPR_SHIFTLEFT:
		return op_Shl;
	case BINEXPR_SHIFTRIGHT:
		return op_Shr;
	default:
		return NULL;
	}
}

static
long binexpr_type_to_cmp_pn(binary_expression_type_t type)
{
	switch(type) {
	case BINEXPR_EQUAL:
		return pn_Cmp_Eq;
	case BINEXPR_NOTEQUAL:
		return pn_Cmp_Lg;
	case BINEXPR_LESS:
		return pn_Cmp_Lt;
	case BINEXPR_LESSEQUAL:
		return pn_Cmp_Le;
	case BINEXPR_GREATER:
		return pn_Cmp_Gt;
	case BINEXPR_GREATEREQUAL:
		return pn_Cmp_Ge;
	default:
		return 0;
	}
}

static
ir_node *binary_expression_to_firm(const binary_expression_t *binary_expression)
{
	switch(binary_expression->type) {
	case BINEXPR_ASSIGN:
		return assign_expression_to_firm(binary_expression);
	/* TODO construct Div,Mod nodes (they need special treatment with memory
	 * edges */
	default:
		break;
	}

	/* an arithmetic binexpressions? */
	ir_op *irop = binexpr_type_to_op(binary_expression->type);
	if(irop != NULL) {
		ir_node *in[2] = {
			expression_to_firm(binary_expression->left),
			expression_to_firm(binary_expression->right)
		};
		ir_mode *mode  = get_ir_mode(binary_expression->expression.datatype);
		ir_node *block = get_cur_block();
		ir_node *node  = new_ir_node(NULL, current_ir_graph, block,
				irop, mode, 2, in);
		return node;
	}

	/* a comparison expression? */
	long compare_pn = binexpr_type_to_cmp_pn(binary_expression->type);
	if(compare_pn != 0) {
		ir_node *left  = expression_to_firm(binary_expression->left);
		ir_node *right = expression_to_firm(binary_expression->right);
		ir_node *cmp   = new_Cmp(left, right);
		ir_node *proj  = new_Proj(cmp, mode_b, compare_pn);
		return proj;
	}

	abort();
}

static
ir_node *cast_expression_to_firm(const unary_expression_t *cast)
{
	ir_node *node      = expression_to_firm(cast->value);
	assert(node != NULL);
	ir_mode *mode      = get_ir_mode(cast->expression.datatype);
	ir_node *conv_node = new_Conv(node, mode);

	return conv_node;
}

static
ir_node *load_from_expression_addr(type_t *type, ir_node *addr)
{
	ir_mode *mode  = get_ir_mode(type);
	ir_node *store = get_store();
	ir_node *load  = new_Load(store, addr, mode);
	ir_node *mem   = new_Proj(load, mode_M, pn_Load_M);
	ir_node *val   = new_Proj(load, mode, pn_Load_res);
	set_store(mem);

	return val;
}

static
ir_node *unary_expression_to_firm(const unary_expression_t *unary_expression)
{
	ir_node *addr;

	switch(unary_expression->type) {
	case UNEXPR_CAST:
		return cast_expression_to_firm(unary_expression);
	case UNEXPR_DEREFERENCE:
		addr = expression_addr(&unary_expression->expression);
		return load_from_expression_addr(unary_expression->expression.datatype,
		                                 addr);
	default:
		abort();
	}
}

static
ir_node *select_expression_to_firm(const select_expression_t *select)
{
	ir_node *addr       = select_expression_addr(select);
	type_t  *entry_type = select->compound_entry->type;
	if(entry_type->type == TYPE_COMPOUND_STRUCT	
			|| entry_type->type == TYPE_COMPOUND_UNION
			|| entry_type->type == TYPE_ARRAY)
		return addr;

	return load_from_expression_addr(select->expression.datatype, addr);
}

static
ir_node *method_reference_to_firm(method_t *method, symbol_t *symbol,
                                  type_argument_t *type_arguments)
{
	int old_top        = typevar_binding_stack_top();
	push_type_variable_bindings(method->type_parameters, type_arguments);

	ir_entity  *entity        = get_method_entity(method, symbol);
	const char *name          = get_entity_name(entity);
	int        needs_instance = is_polymorphic_method(method);

	pop_type_variable_bindings(old_top);

	if(needs_instance) {
		const char *name = get_entity_name(entity);
		if(strset_find(&instantiated_methods, name) != NULL) {
			needs_instance = 0;
		}
	}

	if(needs_instance) {
		instantiate_method_t *instantiate 
			=	queue_method_instantiation(method, symbol);
		instantiate->entity = entity;

		type_argument_t *type_argument = type_arguments;
		type_argument_t *last_argument = NULL;
		while(type_argument != NULL) {
			type_t          *type         = type_argument->type;
			type_argument_t *new_argument
				= obstack_alloc(&obst, sizeof(new_argument[0]));
			memset(new_argument, 0, sizeof(new_argument[0]));

			new_argument->type = create_concrete_type(type);

			if(last_argument != NULL) {
				last_argument->next = new_argument;
			} else {
				instantiate->type_arguments = new_argument;
			}
			last_argument = new_argument;

			type_argument = type_argument->next;
		}

		strset_insert(&instantiated_methods, name);
	}

	ir_node *symconst = new_SymConst((union symconst_symbol) entity,
	                                 symconst_addr_ent);

	return symconst;
}

#if 0
static
ir_node *typeclass_method_instance_reference_to_firm(
		typeclass_method_instance_t *method_instance)
{
	ir_entity *entity   = get_typeclass_method_instance_entity(method_instance);
	ir_node   *symconst	= new_SymConst((union symconst_symbol) entity,
	                                   symconst_addr_ent);
	return symconst;
}
#endif

static
ir_node *typeclass_method_reference_to_firm(typeclass_method_t *method,
                                            type_argument_t *type_arguments)
{
	typeclass_t        *typeclass = method->typeclass;

	int old_top = typevar_binding_stack_top();
	push_type_variable_bindings(typeclass->type_parameters, type_arguments);

	typeclass_instance_t *instance = find_typeclass_instance(typeclass);
	if(instance == NULL) {
		fprintf(stderr, "while looking at method '%s' from '%s'\n",
		        method->declaration.symbol->string,
		        typeclass->declaration.symbol->string);
		print_type(stderr, typeclass->type_parameters->current_type);
		panic("no typeclass instance found in ast2firm phase");
		return NULL;
	}

	typeclass_method_instance_t *method_instance 
		= get_method_from_typeclass_instance(instance, method);
	if(method_instance == NULL) {
		fprintf(stderr, "panic: no method '%s' in instance of typeclass '%s'\n",
		        method->declaration.symbol->string,
		        typeclass->declaration.symbol->string);
		panic("panic");
		return NULL;
	}

	ir_entity *entity   = get_typeclass_method_instance_entity(method_instance);
	ir_node   *symconst = new_SymConst((union symconst_symbol) entity,
	                                   symconst_addr_ent);

	pop_type_variable_bindings(old_top);
	return symconst;
}

static
ir_node *method_parameter_reference_to_firm(method_parameter_t *parameter)
{
	ir_node *args  = get_irg_args(current_ir_graph);
	ir_mode *mode  = get_ir_mode(parameter->type);
	ir_node *block = get_irg_start_block(current_ir_graph);
	long     pn    = parameter->num;
	ir_node *proj  = new_r_Proj(current_ir_graph, block, args, mode, pn);

	return proj;
}

static
ir_node *sizeof_expression_to_firm(const sizeof_expression_t *expression)
{
	ir_mode  *mode = get_ir_mode(expression->expression.datatype);
	unsigned  size = get_type_size(expression->type);
	tarval   *tv   = new_tarval_from_long(size, mode);
	ir_node  *res  = new_Const(mode, tv);

	return res;
}

static
ir_node *call_expression_to_firm(const call_expression_t *call)
{
	expression_t  *method = call->method;
	ir_node       *callee = expression_to_firm(method);

	assert(method->datatype->type == TYPE_POINTER);
	pointer_type_t *pointer_type  = (pointer_type_t*) method->datatype;
	type_t         *points_to     = pointer_type->points_to;

	assert(points_to->type == TYPE_METHOD);
	method_type_t *method_type     = (method_type_t*) points_to;
	ir_type       *ir_method_type  = get_ir_type((type_t*) method_type);
	ir_type       *new_method_type = NULL;

	int              n_parameters = 0;
	call_argument_t *argument     = call->arguments;
	while(argument != NULL) {
		n_parameters++;
		argument = argument->next;
	}

	if(method_type->variable_arguments) {
		/* we need to construct a new method type matching the call
		 * arguments... */
		new_method_type = new_type_method(unique_id("Call."), n_parameters,
		                                  get_method_n_ress(ir_method_type));
		set_method_calling_convention(new_method_type,
		               get_method_calling_convention(ir_method_type));
		set_method_additional_properties(new_method_type,
		               get_method_additional_properties(ir_method_type));

		for(int i = 0; i < get_method_n_ress(ir_method_type); ++i) {
			set_method_res_type(new_method_type, i,
			                    get_method_res_type(ir_method_type, i));
		}
	}
	ir_node *in[n_parameters];

	argument = call->arguments;
	int n = 0;
	while(argument != NULL) {
		expression_t *expression = argument->expression;

		in[n] = expression_to_firm(expression);
		if(new_method_type != NULL) {
			ir_type *irtype = get_ir_type(expression->datatype);
			set_method_param_type(new_method_type, n, irtype);
		}

		argument = argument->next;
		n++;
	}

	if(new_method_type != NULL)
		ir_method_type = new_method_type;

	ir_node *store = get_store();
	ir_node *node  = new_Call(store, callee, n_parameters, in, ir_method_type);
	ir_node *mem   = new_Proj(node, mode_M, pn_Call_M_regular);
	set_store(mem);

	type_t  *result_type = method_type->result_type;
	ir_node *result      = NULL;
	if(result_type->type != TYPE_VOID) {
		ir_mode *mode    = get_ir_mode(result_type);
		ir_node *resproj = new_Proj(node, mode_T, pn_Call_T_result);
		result           = new_Proj(resproj, mode, 0);
	}

	return result;
}

static
ir_node *reference_expression_to_firm(const reference_expression_t *reference)
{
	method_declaration_t *method_declaration;
	declaration_t *declaration = reference->declaration;

	switch(declaration->type) {
	case DECLARATION_METHOD:
		method_declaration = (method_declaration_t*) declaration;
		return method_reference_to_firm(&method_declaration->method,
		                                reference->symbol,
		                                reference->type_arguments);
	case DECLARATION_TYPECLASS_METHOD:
		return typeclass_method_reference_to_firm(
				(typeclass_method_t*) declaration, reference->type_arguments);
	case DECLARATION_METHOD_PARAMETER:
		return method_parameter_reference_to_firm(
				(method_parameter_t*) declaration);
	case DECLARATION_CONSTANT:
		return constant_reference_to_firm((constant_t*) declaration);
	case DECLARATION_GLOBAL_VARIABLE:
		return global_variable_to_firm((global_variable_t*) declaration);
	case DECLARATION_VARIABLE:
		return variable_reference_to_firm(
				(variable_declaration_t*) declaration);
	case DECLARATION_LAST:
	case DECLARATION_INVALID:
	case DECLARATION_TYPEALIAS:
	case DECLARATION_TYPECLASS:
	case DECLARATION_LABEL:
	case DECLARATION_TYPE_VARIABLE:
		panic("internal error: trying to construct node for non-data "
		      "reference");
	}
	panic("unknown declaration type found");
}

static
ir_node *expression_to_firm(const expression_t *expression)
{
	ir_node *addr;

	switch(expression->type) {
	case EXPR_INT_CONST:
		return int_const_to_firm((const int_const_t*) expression);
	case EXPR_STRING_CONST:
		return string_const_to_firm((const string_const_t*) expression);
	case EXPR_NULL_POINTER:
		return null_pointer_to_firm();
	case EXPR_REFERENCE:
		return reference_expression_to_firm(
				(const reference_expression_t*) expression);
	case EXPR_BINARY:
		return binary_expression_to_firm(
				(const binary_expression_t*) expression);
	case EXPR_UNARY:
		return unary_expression_to_firm(
				(const unary_expression_t*) expression);
	case EXPR_SELECT:
		return select_expression_to_firm(
				(const select_expression_t*) expression);
	case EXPR_ARRAY_ACCESS:
		addr = expression_addr(expression);
		return load_from_expression_addr(expression->datatype, addr);
	case EXPR_CALL:
		return call_expression_to_firm((const call_expression_t*) expression);
	case EXPR_SIZEOF:
		return sizeof_expression_to_firm(
				(const sizeof_expression_t*) expression);
	case EXPR_LAST:
	case EXPR_INVALID:
		break;
	}
	abort();
	return NULL;
}




static
void statement_to_firm(statement_t *statement);

static
void return_statement_to_firm(const return_statement_t *statement)
{
	ir_node *ret;
	if(statement->return_value != NULL) {
		ir_node *retval = expression_to_firm(statement->return_value);
		ir_node *in[1];

		in[0] = retval;
		ret = new_Return(get_store(), 1, in);
	} else {
		ret = new_Return(get_store(), 0, NULL);
	}
	ir_node *end_block = get_irg_end_block(current_ir_graph);
	add_immBlock_pred(end_block, ret);

	set_cur_block(NULL);
}

static
void if_statement_to_firm(const if_statement_t *statement)
{
	ir_node *condition = expression_to_firm(statement->condition);
	assert(condition != NULL);

	ir_node *cond       = new_Cond(condition);
	ir_node *true_proj  = new_Proj(cond, mode_X, pn_Cond_true);
	ir_node *false_proj = new_Proj(cond, mode_X, pn_Cond_false);

	ir_node *fallthrough_block = new_immBlock();

	/* the true (blocks) */
	ir_node *true_block = new_immBlock();
	add_immBlock_pred(true_block, true_proj);
	mature_immBlock(true_block);

	statement_to_firm(statement->true_statement);
	if(get_cur_block() != NULL) {
		ir_node *jmp = new_Jmp();
		add_immBlock_pred(fallthrough_block, jmp);
	}

	/* the false (blocks) */
	if(statement->false_statement != NULL) {
		ir_node *false_block = new_immBlock();
		add_immBlock_pred(false_block, false_proj);
		mature_immBlock(false_block);

		statement_to_firm(statement->false_statement);
		if(get_cur_block() != NULL) {
			ir_node *jmp = new_Jmp();
			add_immBlock_pred(fallthrough_block, jmp);
		}
	} else {
		add_immBlock_pred(fallthrough_block, false_proj);
	}
	mature_immBlock(fallthrough_block);

	set_cur_block(fallthrough_block);
}

static
void expression_statement_to_firm(const expression_statement_t *statement)
{
	expression_to_firm(statement->expression);
}

static
void block_statement_to_firm(const block_statement_t *block)
{
	statement_t *statement = block->statements;
	while(statement != NULL) {
		statement_to_firm(statement);
		statement = statement->next;
	}
}

static
void goto_statement_to_firm(goto_statement_t *goto_statement)
{
	label_declaration_t *label = goto_statement->label;
	ir_node             *block = label->block;

	if(block == NULL) {
		ir_node *temp = get_cur_block();
		block        = new_immBlock();
		set_cur_block(temp);
		label->block = block;
		label->next  = labels;
		labels       = label;
	}
	ir_node *jmp = new_Jmp();
	add_immBlock_pred(block, jmp);

	set_cur_block(NULL);
}

static
void label_statement_to_firm(label_statement_t *label_statement)
{
	label_declaration_t *label = &label_statement->declaration;
	ir_node *block = label->block;

	if(block == NULL) {
		ir_node *temp = get_cur_block();
		block         = new_immBlock();
		label->block  = block;
		label->next   = labels;
		labels        = label;

		set_cur_block(temp);
	}

	if(get_cur_block() != NULL) {
		ir_node *jmp = new_Jmp();

		add_immBlock_pred(block, jmp);
	}
	set_cur_block(block);
}

static
void statement_to_firm(statement_t *statement)
{
	if(statement->type != STATEMENT_LABEL && get_cur_block() == NULL) {
		fprintf(stderr, "Warning: unreachable code detected\n");
		return;
	}

	switch(statement->type) {
	case STATEMENT_BLOCK:
		block_statement_to_firm((block_statement_t*) statement);
		return;
	case STATEMENT_RETURN:
		return_statement_to_firm((return_statement_t*) statement);
		return;
	case STATEMENT_IF:
		if_statement_to_firm((if_statement_t*) statement);
		return;
	case STATEMENT_VARIABLE_DECLARATION:
		/* nothing to do */
		break;
	case STATEMENT_EXPRESSION:
		expression_statement_to_firm((expression_statement_t*) statement);
		break;
	case STATEMENT_LABEL:
		label_statement_to_firm((label_statement_t*) statement);
		break;
	case STATEMENT_GOTO:
		goto_statement_to_firm((goto_statement_t*) statement);
		break;
	default:
		abort();
	}
}

static
void create_method(method_t *method, symbol_t *symbol,
                   type_argument_t *type_arguments)
{
	if(method->is_extern)
		return;

	int old_top = typevar_binding_stack_top();
	if(is_polymorphic_method(method)) {
		assert(type_arguments != NULL);
		push_type_variable_bindings(method->type_parameters, type_arguments);
	}
	
	ir_entity *entity = get_method_entity(method, symbol);
	ir_graph *irg     = new_ir_graph(entity, method->n_local_vars);

	assert(value_numbers == NULL);
	value_numbers = xmalloc(method->n_local_vars * sizeof(value_numbers[0]));

	ir_node *firstblock = get_cur_block();

	if(method->statement)
		statement_to_firm(method->statement);

	/* no return statement seen yet? */
	ir_node *end_block = get_irg_end_block(irg);
	if(get_cur_block() != NULL) {
		ir_node *ret = new_Return(get_store(), 0, NULL);
		add_immBlock_pred(end_block, ret);
	}

	mature_immBlock(firstblock);
	mature_immBlock(end_block);

	label_declaration_t *label = labels;
	while(label != NULL) {
		mature_immBlock(label->block);
		label = label->next;
	}
	labels = NULL;

	irg_finalize_cons(irg);

	ir_type *frame_type = get_irg_frame_type(irg);
	set_type_size_bytes(frame_type, 0);
	set_type_alignment_bytes(frame_type, 4);
	set_type_state(frame_type, layout_fixed);

	irg_vrfy(irg);

	free(value_numbers);
	value_numbers = NULL;

	pop_type_variable_bindings(old_top);
}

static
void create_typeclass_instance(typeclass_instance_t *instance)
{
	typeclass_method_instance_t *method_instance = instance->method_instances;
	while(method_instance != NULL) {
		method_t *method = & method_instance->method;
		/* make sure the method entity is set */
		get_typeclass_method_instance_entity(method_instance);
		/* we can emit it like a normal method */
		create_method(method, method_instance->symbol, NULL);

		method_instance  = method_instance->next;
	}
}

/**
 * Build a firm representation of an AST programm
 */
void ast2firm(namespace_t *namespace)
{
	method_declaration_t *method_declaration;
	method_t             *method;

	obstack_init(&obst);
	strset_init(&instantiated_methods);
	typevar_binding_stack = NEW_ARR_F(typevar_binding_t, 0);
	instantiate_methods   = new_pdeq();

	/* scan compilation unit for functions */
	declaration_t *declaration = namespace->context.declarations;
	while(declaration != NULL) {
		switch(declaration->type) {
		case DECLARATION_METHOD:
			method_declaration = (method_declaration_t*) declaration;
			method             = &method_declaration->method;

			if(!is_polymorphic_method(method)) {
				create_method(method, method_declaration->declaration.symbol,
				              NULL);
			}
			break;
		case DECLARATION_GLOBAL_VARIABLE:
			create_global_variable_entity((global_variable_t*) declaration);
			break;
		case DECLARATION_VARIABLE:
		case DECLARATION_TYPEALIAS:
		case DECLARATION_TYPECLASS:
		case DECLARATION_CONSTANT:
		case DECLARATION_LABEL:
		case DECLARATION_METHOD_PARAMETER:
		case DECLARATION_TYPECLASS_METHOD:
		case DECLARATION_TYPE_VARIABLE:
			break;
		case DECLARATION_LAST:
		case DECLARATION_INVALID:
			panic("Invalid namespace entry type found");
		}

		declaration = declaration->next;
	}

	typeclass_instance_t *instance = namespace->context.typeclass_instances;
	while(instance != NULL) {
		create_typeclass_instance(instance);
		instance = instance->next;
	}

	while(!pdeq_empty(instantiate_methods)) {
		instantiate_method_t *instantiate_method 
			= pdeq_getl(instantiate_methods);

		create_method(instantiate_method->method,
		              instantiate_method->symbol,
		              instantiate_method->type_arguments);
	}

	del_pdeq(instantiate_methods);
	DEL_ARR_F(typevar_binding_stack);
	obstack_free(&obst, NULL);
	strset_destroy(&instantiated_methods);
}

