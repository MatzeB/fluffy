#include <config.h>

#include <assert.h>
#include <string.h>

#include <libfirm/firm.h>

#include "ast_t.h"
#include "type_t.h"
#include "semantic_t.h"
#include "mangle.h"
#include "adt/array.h"
#include "adt/obst.h"
#include "adt/strset.h"
#include "adt/error.h"
#include "adt/xmalloc.h"
#include <libfirm/adt/pdeq.h>

static const variable_declaration_t **value_numbers    = NULL;
static label_declaration_t           *labels           = NULL;
/** context for the variables, this is usually the stack frame but might
 * be something else for things like coroutines */
static ir_node                       *variable_context = NULL;

typedef struct instantiate_method_t  instantiate_method_t;

static ir_type *byte_ir_type  = NULL;
static ir_type *void_ptr_type = NULL;
static type_t  *type_bool     = NULL;

struct instantiate_method_t {
	method_t             *method;
	ir_entity            *entity;
	type_argument_t      *type_arguments;
};

typedef struct type2firm_env_t type2firm_env_t;
struct type2firm_env_t {
	int can_cache;       /* nonzero if type can safely be cached because
	                        no typevariables are in the hierarchy */
};

static struct obstack     obst;
static strset_t           instantiated_methods;
static pdeq              *instantiate_methods   = NULL;

static ir_type *_get_ir_type(type2firm_env_t *env, type_t *type);
static ir_type *get_ir_type(type_t *type);
static void context2firm(const context_t *context);

ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos)
{
	const declaration_t *declaration 
		= (const declaration_t*) &value_numbers[pos];

	print_warning_prefix(declaration->base.source_position);
	fprintf(stderr, "variable '%s' might be used uninitialized\n",
			declaration->base.symbol->string);
	return new_r_Unknown(irg, mode);
}

unsigned dbg_snprint(char *buf, unsigned len, const dbg_info *dbg)
{
	const source_position_t *pos = (const source_position_t*) dbg;
	if (pos == NULL)
		return 0;
	return (unsigned) snprintf(buf, len, "%s:%u", pos->input_name,
	                           pos->linenr);
}

const char *dbg_retrieve(const dbg_info *dbg, unsigned *line)
{
	const source_position_t *pos = (const source_position_t*) dbg;
	if (pos == NULL)
		return NULL;
	if (line != NULL)
		*line = pos->linenr;
	return pos->input_name;
}

void init_ast2firm(void)
{
}

static void init_ir_types(void)
{
	type_bool = make_atomic_type(ATOMIC_TYPE_BOOL);

	atomic_type_t byte_type;
	memset(&byte_type, 0, sizeof(byte_type));
	byte_type.base.kind = TYPE_ATOMIC;
	byte_type.akind     = ATOMIC_TYPE_BYTE;

	byte_ir_type = get_ir_type((type_t*) &byte_type);

	ir_type *ir_type_void = get_ir_type(type_void);
	void_ptr_type         = new_type_pointer(new_id_from_str("void_ptr"),
	                                         ir_type_void, mode_P_data);
}

void exit_ast2firm(void)
{
}

static unsigned unique_id = 0;

static ident *unique_ident(const char *tag)
{
	char buf[256];

	snprintf(buf, sizeof(buf), "%s.%d", tag, unique_id);
	unique_id++;
	return new_id_from_str(buf);
}

static symbol_t *unique_symbol(const char *tag)
{
	obstack_printf(&symbol_obstack, "%s.%d", tag, unique_id);
	unique_id++;

	const char *string = obstack_finish(&symbol_obstack);
	symbol_t   *symbol = symbol_table_insert(string);

	assert(symbol->string == string);

	return symbol;
}

static ir_mode *get_atomic_mode(const atomic_type_t* atomic_type)
{
	switch (atomic_type->akind) {
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
		return mode_b;
	case ATOMIC_TYPE_INVALID:
		break;
	}
	panic("Encountered unknown atomic type");
}


static unsigned get_type_size(type_t *type);

static unsigned get_atomic_type_size(const atomic_type_t *type)
{
	switch (type->akind) {
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

static unsigned get_compound_type_size(compound_type_t *type)
{
	ir_type *irtype = get_ir_type((type_t*) type);
	return get_type_size_bytes(irtype);
}

static unsigned get_type_reference_type_var_size(const type_reference_t *type)
{
	type_variable_t *type_variable = type->type_variable;

	if (type_variable->current_type == NULL) {
		panic("taking size of unbound type variable");
		return 0;
	}
	return get_type_size(type_variable->current_type);
}

static unsigned get_array_type_size(array_type_t *type)
{
	ir_type *irtype = get_ir_type((type_t*) type);
	return get_type_size_bytes(irtype);
}

static unsigned get_type_size(type_t *type)
{
	switch (type->kind) {
	case TYPE_VOID:
		return 0;
	case TYPE_ATOMIC:
		return get_atomic_type_size((const atomic_type_t*) type);
	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		return get_compound_type_size((compound_type_t*) type);
	case TYPE_FUNCTION:
		/* just a pointer to the method */
		return get_mode_size_bytes(mode_P_code);
	case TYPE_POINTER:
		return get_mode_size_bytes(mode_P_data);
	case TYPE_ARRAY:
		return get_array_type_size((array_type_t*) type);
	case TYPE_TYPEOF: {
		const typeof_type_t *typeof_type = (const typeof_type_t*) type;
		return get_type_size(typeof_type->expression->base.type);
	}
	case TYPE_REFERENCE:
		panic("Type reference not resolved");
		break;
	case TYPE_REFERENCE_TYPE_VARIABLE:
		return get_type_reference_type_var_size((type_reference_t*) type);
	case TYPE_ERROR:
		return 0;
	case TYPE_INVALID:
		break;
	case TYPE_BIND_TYPEVARIABLES:
		abort();
	}
	panic("Trying to determine size of invalid type");
}

static int count_parameters(const function_type_t *function_type)
{
	int count = 0;

	function_parameter_type_t *param_type = function_type->parameter_types;
	while (param_type != NULL) {
		param_type = param_type->next;
		count++;
	}

	return count;
}

static ir_type *get_atomic_type(type2firm_env_t *env, const atomic_type_t *type)
{
	(void) env;
	ir_mode *mode   = get_atomic_mode(type);
	ident   *id     = get_mode_ident(mode);
	ir_type *irtype = new_type_primitive(id, mode);

	return irtype;
}

static ir_type *get_function_type(type2firm_env_t *env,
                                  const function_type_t *function_type)
{
	type_t  *result_type  = function_type->result_type;

	ident   *id           = unique_ident("functiontype");
	int      n_parameters = count_parameters(function_type);
	int      n_results    = result_type->kind == TYPE_VOID ? 0 : 1;
	ir_type *irtype       = new_type_method(id, n_parameters, n_results);

	if (result_type->kind != TYPE_VOID) {
		ir_type *restype = _get_ir_type(env, result_type);
		set_method_res_type(irtype, 0, restype);
	}

	function_parameter_type_t *param_type = function_type->parameter_types;
	int n = 0;
	while (param_type != NULL) {
		ir_type *p_irtype = _get_ir_type(env, param_type->type);
		set_method_param_type(irtype, n, p_irtype);

		param_type = param_type->next;
		n++;
	}

	if (function_type->variable_arguments) {
		set_method_variadicity(irtype, variadicity_variadic);
	}

	return irtype;
}

static ir_type *get_pointer_type(type2firm_env_t *env, pointer_type_t *type)
{
	type_t  *points_to = type->points_to;
	ir_type *ir_points_to;
	/* Avoid endless recursion if the points_to type contains this poiner type
	 * again (might be a struct). We therefore first create a void* pointer
	 * and then set the real points_to type
	 */
	ir_type *ir_type_void = get_ir_type(type_void);
	ir_type *ir_type      = new_type_pointer(unique_ident("pointer"),
                                             ir_type_void, mode_P_data);
	type->base.firm_type  = ir_type;

	ir_points_to = _get_ir_type(env, points_to);
	set_pointer_points_to_type(ir_type, ir_points_to);

	return ir_type;
}

static ir_node *expression_to_firm(expression_t *expression);

static tarval *fold_constant_to_tarval(expression_t *expression)
{
	assert(is_constant_expression(expression));

	ir_graph *old_current_ir_graph = current_ir_graph;
	current_ir_graph = get_const_code_irg();

	ir_node *cnst = expression_to_firm(expression);
	current_ir_graph = old_current_ir_graph;

	if (!is_Const(cnst)) {
		panic("couldn't fold constant");
	}

	tarval* tv = get_Const_tarval(cnst);
	return tv;
}

long fold_constant_to_int(expression_t *expression)
{
	if (expression->kind == EXPR_ERROR)
		return 0;

	tarval *tv = fold_constant_to_tarval(expression);
	if (!tarval_is_long(tv)) {
		panic("result of constant folding is not an integer");
	}

	return get_tarval_long(tv);
}

static ir_type *get_array_type(type2firm_env_t *env, array_type_t *type)
{
	type_t  *element_type    = type->element_type;
	ir_type *ir_element_type = _get_ir_type(env, element_type);

	ir_type *ir_type = new_type_array(unique_ident("array"), 1, ir_element_type);
	int size = fold_constant_to_int(type->size_expression);
	set_array_bounds_int(ir_type, 0, 0, size);

	size_t elemsize = get_type_size_bytes(ir_element_type);
	int align = get_type_alignment_bytes(ir_element_type);
	if (elemsize % align > 0) {
		elemsize += align - (elemsize % align);
	}
	set_type_size_bytes(ir_type, size * elemsize);
	set_type_alignment_bytes(ir_type, align);
	set_type_state(ir_type, layout_fixed);

	return ir_type;
}

#define INVALID_TYPE ((ir_type_ptr)-1)

static ir_type *get_struct_type(type2firm_env_t *env, compound_type_t *type)
{
	symbol_t *symbol = type->symbol;
	ident    *id;
	if (symbol != NULL) {
		id = unique_ident(symbol->string);
	} else {
		id = unique_ident("__anonymous_struct");
	}
	ir_type *ir_type = new_type_struct(id);

	type->base.firm_type = ir_type;

	int align_all = 1;
	int offset    = 0;
	compound_entry_t *entry = type->entries;
	while (entry != NULL) {
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
		if (entry_alignment > align_all) {
			if (entry_alignment % align_all != 0) {
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

static ir_type *get_union_type(type2firm_env_t *env, compound_type_t *type)
{
	symbol_t *symbol = type->symbol;
	ident    *id;
	if (symbol != NULL) {
		id = unique_ident(symbol->string);
	} else {
		id = unique_ident("__anonymous_union");
	}
	ir_type  *ir_type = new_type_union(id);

	type->base.firm_type = ir_type;

	int align_all = 1;
	int size      = 0;
	compound_entry_t *entry = type->entries;
	while (entry != NULL) {
		ident       *ident         = new_id_from_str(entry->symbol->string);
		ir_type_ptr  entry_ir_type = _get_ir_type(env, entry->type);

		int entry_size      = get_type_size_bytes(entry_ir_type);
		int entry_alignment = get_type_alignment_bytes(entry_ir_type);

		ir_entity *entity = new_entity(ir_type, ident, entry_ir_type);
		add_union_member(ir_type, entity);
		set_entity_offset(entity, 0);
		entry->entity = entity;

		if (entry_size > size) {
			size = entry_size;
		}
		if (entry_alignment > align_all) {
			if (entry_alignment % align_all != 0) {
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

static ir_type *get_type_for_type_variable(type2firm_env_t *env,
                                           type_reference_t *ref)
{
	assert(ref->base.kind == TYPE_REFERENCE_TYPE_VARIABLE);
	type_variable_t *type_variable = ref->type_variable;
	type_t          *current_type  = type_variable->current_type;

	if (current_type == NULL) {
		fprintf(stderr, "Panic: trying to transform unbound type variable "
		        "'%s'\n", type_variable->base.symbol->string);
		abort();
	}
	ir_type *ir_type = _get_ir_type(env, current_type);
	env->can_cache   = 0;

	return ir_type;
}

static ir_type *get_type_for_bind_typevariables(type2firm_env_t *env,
                                                bind_typevariables_type_t *type)
{
	compound_type_t *polymorphic_type = type->polymorphic_type;

	int old_top = typevar_binding_stack_top();
	push_type_variable_bindings(polymorphic_type->type_parameters,
	                            type->type_arguments);

	ir_type *result = _get_ir_type(env, (type_t*) polymorphic_type);

	pop_type_variable_bindings(old_top);

	return result;
}

static ir_type *_get_ir_type(type2firm_env_t *env, type_t *type)
{
	assert(type != NULL);

	if (type->base.firm_type != NULL) {
		assert(type->base.firm_type != INVALID_TYPE);
		return type->base.firm_type;
	}

	ir_type *firm_type = NULL;
	switch (type->kind) {
	case TYPE_ATOMIC:
		firm_type = get_atomic_type(env, &type->atomic);
		break;
	case TYPE_FUNCTION:
		firm_type = get_function_type(env, &type->function);
		break;
	case TYPE_POINTER:
		firm_type = get_pointer_type(env, &type->pointer);
		break;
	case TYPE_ARRAY:
		firm_type = get_array_type(env, &type->array);
		break;
	case TYPE_VOID:
		/* there is no mode_VOID in firm, use mode_C */
		firm_type = new_type_primitive(new_id_from_str("void"), mode_ANY);
		break;
	case TYPE_COMPOUND_STRUCT:
		firm_type = get_struct_type(env, &type->compound);
		break;
	case TYPE_COMPOUND_UNION:
		firm_type = get_union_type(env, &type->compound);
		break;
	case TYPE_REFERENCE_TYPE_VARIABLE:
		firm_type = get_type_for_type_variable(env, &type->reference);
		break;
	case TYPE_BIND_TYPEVARIABLES:
		firm_type = get_type_for_bind_typevariables(env,
		                                            &type->bind_typevariables);
		break;
	case TYPE_TYPEOF: {
		typeof_type_t *typeof_type = (typeof_type_t*) type;
		firm_type = get_ir_type(typeof_type->expression->base.type);
		break;
	}
	case TYPE_REFERENCE:
		panic("unresolved reference type found");
		break;
	case TYPE_ERROR:
	case TYPE_INVALID:
		break;
	}
	if (firm_type == NULL)
		panic("unknown type found");

	if (env->can_cache) {
		type->base.firm_type = firm_type;
	}
	return firm_type;

}

static ir_type *get_ir_type(type_t *type)
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

static instantiate_method_t *queue_method_instantiation(method_t  *method,
                                                 ir_entity *entity)
{
	instantiate_method_t *instantiate
		= obstack_alloc(&obst, sizeof(instantiate[0]));
	memset(instantiate, 0, sizeof(instantiate[0]));

	instantiate->method = method;
	instantiate->entity = entity;
	pdeq_putr(instantiate_methods, instantiate);

	return instantiate;
}

static int is_polymorphic_method(const method_t *method)
{
	return method->type_parameters != NULL;
}

static ir_entity* get_concept_method_instance_entity(
		concept_method_instance_t *method_instance)
{
	method_t *method = & method_instance->method;
	if (method->e.entity != NULL)
		return method->e.entity;

	function_type_t *function_type = method->type;

	concept_method_t *concept_method = method_instance->concept_method;
	concept_t        *concept        = concept_method->concept;

	start_mangle();
	mangle_concept_name(concept->base.symbol);
	mangle_symbol(concept_method->base.symbol);

	concept_instance_t *instance = method_instance->concept_instance;
	type_argument_t    *argument = instance->type_arguments;
	for ( ; argument != NULL; argument = argument->next) {
		mangle_type(argument->type);
	}
	ident *id = finish_mangle();

	/* create the entity */
	ir_type *global_type    = get_glob_type();
	ir_type *ir_method_type = get_ir_type((type_t*) function_type);

	ir_entity *entity       = new_entity(global_type, id, ir_method_type);
	set_entity_ld_ident(entity, id);
	set_entity_visibility(entity, visibility_local);

	method->e.entity = entity;

	return entity;
}

static ir_entity* get_method_entity(method_t *method, symbol_t *symbol)
{
	function_type_t *function_type  = method->type;
	int              is_polymorphic = is_polymorphic_method(method);

	if (!is_polymorphic && method->e.entity != NULL) {
		return method->e.entity;
	}

	start_mangle();
	mangle_symbol_simple(symbol);
	if (is_polymorphic) {
		type_variable_t *type_variable = method->type_parameters;
		for ( ; type_variable != NULL; type_variable = type_variable->next) {
			mangle_type(type_variable->current_type);
		}
	}
	ident *id = finish_mangle();

	/* search for an existing entity */
	if (is_polymorphic && method->e.entities != NULL) {
		int len = ARR_LEN(method->e.entities);
		for (int i = 0; i < len; ++i) {
			ir_entity *entity = method->e.entities[i];
			if (get_entity_ident(entity) == id) {
				return entity;
			}
		}
	}

	/* create the entity */
	ir_type *global_type    = get_glob_type();
	ir_type *ir_method_type = get_ir_type((type_t*) function_type);

	ir_entity *entity       = new_entity(global_type, id, ir_method_type);
	set_entity_ld_ident(entity, id);
	if (method->is_extern) {
		set_entity_visibility(entity, visibility_external_allocated);
	} else {
		set_entity_visibility(entity, visibility_local);
	}

	if (!is_polymorphic) {
		method->e.entity = entity;
	} else {
		if (method->e.entities == NULL)
			method->e.entities = NEW_ARR_F(ir_entity*, 0);
		ARR_APP1(ir_entity*, method->e.entities, entity);
	}
	return entity;
}

static dbg_info* get_dbg_info(const source_position_t *pos)
{
	return (dbg_info*) pos;
}

static ir_node *load_from_expression_addr(type_t *type, ir_node *addr,
                                          const source_position_t *pos);

static ir_node *int_const_to_firm(const int_const_t *cnst)
{
	ir_mode  *mode = get_ir_mode(cnst->base.type);
	tarval   *tv   = new_tarval_from_long(cnst->value, mode);
	dbg_info *dbgi = get_dbg_info(&cnst->base.source_position);

	return new_d_Const(dbgi, tv);
}

static ir_node *float_const_to_firm(const float_const_t *cnst)
{
	ir_mode  *mode = get_ir_mode(cnst->base.type);
	tarval   *tv   = new_tarval_from_double(cnst->value, mode);
	dbg_info *dbgi = get_dbg_info(&cnst->base.source_position);

	return new_d_Const(dbgi, tv);
}

static ir_node *bool_const_to_firm(const bool_const_t *cnst)
{
	dbg_info *dbgi = get_dbg_info(&cnst->base.source_position);

	if (cnst->value == 0) {
		return new_d_Const(dbgi, get_tarval_b_false());
	} else {
		return new_d_Const(dbgi, get_tarval_b_true());
	}
}

static ir_node *string_const_to_firm(const string_const_t* cnst)
{
	ir_type   *global_type = get_glob_type();
	ir_type   *type        = new_type_array(unique_ident("bytearray"), 1,
	                                        byte_ir_type);

	ir_entity *ent = new_entity(global_type, unique_ident("str"), type);
	set_entity_variability(ent, variability_constant);
	set_entity_allocation(ent, allocation_static);
	set_entity_visibility(ent, visibility_local);

	ir_type    *elem_type = byte_ir_type;
	ir_mode    *mode      = get_type_mode(elem_type);

	const char *string = cnst->value;
	size_t      slen   = strlen(string) + 1;

	set_array_lower_bound_int(type, 0, 0);
	set_array_upper_bound_int(type, 0, slen);
	set_type_size_bytes(type, slen);
	set_type_state(type, layout_fixed);

	tarval    **tvs = xmalloc(slen * sizeof(tvs[0]));
	for (size_t i = 0; i < slen; ++i) {
		tvs[i] = new_tarval_from_long(string[i], mode);
	}

	set_array_entity_values(ent, tvs, slen);
	free(tvs);

	dbg_info *dbgi = get_dbg_info(&cnst->base.source_position);

	return new_d_SymConst(dbgi, mode_P, (union symconst_symbol) ent, symconst_addr_ent);
}

static ir_node *null_pointer_to_firm(void)
{
	ir_mode *mode = get_type_mode(void_ptr_type);
	tarval  *tv   = get_tarval_null(mode);

	return new_Const(tv);
}

static ir_node *select_expression_addr(const select_expression_t *select)
{
	expression_t *compound_ptr = select->compound;
	/* make sure the firm type for the struct is constructed */
	get_ir_type(compound_ptr->base.type);

	ir_node   *compound_ptr_node = expression_to_firm(compound_ptr);
	ir_node   *nomem             = new_NoMem();
	ir_entity *entity;
	if (select->compound_entry != NULL) {
		entity = select->compound_entry->entity;
	} else {
		// TODO
	}

	dbg_info *dbgi = get_dbg_info(&select->base.source_position);
	ir_node  *addr = new_d_simpleSel(dbgi, nomem, compound_ptr_node, entity);

	return addr;
}

static ir_node *array_access_expression_addr(const array_access_expression_t* access)
{
	expression_t *array_ref  = access->array_ref;
	expression_t *index      = access->index;
	ir_node      *base_addr  = expression_to_firm(array_ref);
	ir_node      *index_node = expression_to_firm(index);

	int           elem_size       = get_type_size(access->base.type);
	tarval       *elem_size_tv    = new_tarval_from_long(elem_size, mode_Is);
	ir_node      *elem_size_const = new_Const(elem_size_tv);
	dbg_info     *dbgi 
		= get_dbg_info(&access->base.source_position);

	ir_node      *mul = new_d_Mul(dbgi, index_node, elem_size_const, mode_Is);
	ir_node      *add = new_d_Add(dbgi, base_addr, mul, mode_P_data);
	return add;
}

static ir_entity *create_variable_entity(variable_declaration_t *variable)
{
	if (variable->entity != NULL)
		return variable->entity;

	ir_type       *parent_type;
	if (variable->is_global) {
		parent_type = get_glob_type();
	} else if (variable->needs_entity) {
		parent_type = get_irg_frame_type(current_ir_graph);
	} else {
		return NULL;
	}

	obstack_printf(&obst, "_%s", variable->base.symbol->string);
	obstack_1grow(&obst, 0);

	char *str = obstack_finish(&obst);
	ident   *ident    = new_id_from_str(str);
	obstack_free(&obst, str);

	type_t  *type     = variable->type;
	ir_type *irtype   = get_ir_type(type);

	ir_entity *entity = new_entity(parent_type, ident, irtype);
	set_entity_ld_ident(entity, ident);
	set_entity_variability(entity, variability_uninitialized);
	set_entity_allocation(entity, allocation_static);
	if (variable->is_extern) {
		set_entity_visibility(entity, visibility_external_allocated);
	} else {
		set_entity_visibility(entity, visibility_local);
	}

	variable->entity = entity;
	return entity;
}

static ir_node *variable_addr(variable_declaration_t *variable)
{
	ir_entity *entity = create_variable_entity(variable);
	dbg_info  *dbgi   = get_dbg_info(&variable->base.source_position);

	ir_node *result;

	if (variable->is_global) {
		result = new_d_SymConst(dbgi, mode_P, (union symconst_symbol) entity,
		                        symconst_addr_ent);
	} else {
		assert(variable->needs_entity);

		ir_node *nomem = new_NoMem();

		result = new_d_simpleSel(dbgi, nomem, variable_context, entity);
	}

	return result;
}

static ir_node *variable_to_firm(variable_declaration_t *variable,
                          const source_position_t *source_position)
{
	if (variable->is_global || variable->needs_entity) {
		ir_node *addr = variable_addr(variable);
		type_t  *type = variable->type;

		if (type->kind == TYPE_COMPOUND_STRUCT 
				|| type->kind == TYPE_COMPOUND_UNION
				|| type->kind == TYPE_BIND_TYPEVARIABLES
				|| type->kind == TYPE_ARRAY) {
			return addr;
		}

		return load_from_expression_addr(type, addr, source_position);
	} else {
		ir_mode *mode = get_ir_mode(variable->type);

		assert(variable->value_number < get_irg_n_locs(current_ir_graph));
		value_numbers[variable->value_number] = variable;

		dbg_info *dbgi = get_dbg_info(source_position);
		return get_d_value(dbgi, variable->value_number, mode);
	}
}

static ir_node *constant_reference_to_firm(const constant_t *constant)
{
	return expression_to_firm(constant->expression);
}

static ir_node *declaration_addr(declaration_t *declaration)
{
	switch (declaration->kind) {
	case DECLARATION_VARIABLE:
		return variable_addr((variable_declaration_t*) declaration);

	case DECLARATION_INVALID:
	case DECLARATION_ERROR:
	case DECLARATION_METHOD:
	case DECLARATION_METHOD_PARAMETER:
	case DECLARATION_ITERATOR:
	case DECLARATION_CONSTANT:
	case DECLARATION_LABEL:
	case DECLARATION_TYPEALIAS:
	case DECLARATION_CONCEPT:
	case DECLARATION_CONCEPT_METHOD:
	case DECLARATION_TYPE_VARIABLE:
		panic("internal error: trying to create address nodes for non-lvalue");
	}
	panic("Unknown declaration found in reference expression");
}

static ir_node *reference_expression_addr(const reference_expression_t *reference)
{
	declaration_t *declaration = reference->declaration;
	return declaration_addr(declaration);
}

static ir_node *expression_addr(const expression_t *expression)
{

	switch (expression->kind) {
	case EXPR_SELECT: {
		const select_expression_t *select
			= (const select_expression_t*) expression;
		return select_expression_addr(select);
	}
	case EXPR_ARRAY_ACCESS:
		return array_access_expression_addr(
				(const array_access_expression_t*) expression);
	case EXPR_REFERENCE:
		return reference_expression_addr(
				(const reference_expression_t*) expression);
	case EXPR_UNARY_DEREFERENCE: {
		const unary_expression_t *unexpr 
			= (const unary_expression_t*) expression;
		return expression_to_firm(unexpr->value);
	}
	default:
		break;
	}
	panic("trying to get address from non lvalue construct");
}

static void firm_assign(expression_t *dest_expr, ir_node *value,
                        const source_position_t *source_position)
{
	if (dest_expr->kind == EXPR_REFERENCE) {
		const reference_expression_t *ref 
			= (const reference_expression_t*) dest_expr;
		declaration_t *declaration = ref->declaration;

		if (declaration->kind == DECLARATION_VARIABLE) {
			variable_declaration_t *variable 
				= (variable_declaration_t*) declaration;

			if (!variable->is_global && !variable->needs_entity) {
				value_numbers[variable->value_number] = variable;
				set_value(variable->value_number, value);
				return;
			}
		}
	}

	ir_node  *addr  = expression_addr(dest_expr);
	ir_node  *store = get_store();
	dbg_info *dbgi  = get_dbg_info(source_position);
	type_t   *type  = dest_expr->base.type;
	ir_node  *result;

	if (type->kind == TYPE_COMPOUND_STRUCT 
	         || type->kind == TYPE_COMPOUND_UNION) {
		ir_type *irtype = get_ir_type(type);

		result       = new_d_CopyB(dbgi, store, addr, value, irtype);
		ir_node *mem = new_d_Proj(dbgi, result, mode_M, pn_CopyB_M_regular);
		set_store(mem);
	} else {
		result        = new_d_Store(dbgi, store, addr, value, cons_none);
		ir_node  *mem = new_d_Proj(dbgi, result, mode_M, pn_Store_M);
		set_store(mem);
	}
}

static ir_node *assign_expression_to_firm(const binary_expression_t *assign)
{
	expression_t *left  = assign->left;
	expression_t *right = assign->right;
	ir_node      *value = expression_to_firm(right);

	firm_assign(left, value, & assign->base.source_position);

	return value;
}

static long binexpr_kind_to_cmp_pn(expression_kind_t kind)
{
	switch (kind) {
	case EXPR_BINARY_EQUAL:
		return pn_Cmp_Eq;
	case EXPR_BINARY_NOTEQUAL:
		return pn_Cmp_Lg;
	case EXPR_BINARY_LESS:
		return pn_Cmp_Lt;
	case EXPR_BINARY_LESSEQUAL:
		return pn_Cmp_Le;
	case EXPR_BINARY_GREATER:
		return pn_Cmp_Gt;
	case EXPR_BINARY_GREATEREQUAL:
		return pn_Cmp_Ge;
	default:
		return 0;
	}
}

static ir_node *create_lazy_op(const binary_expression_t *binary_expression)
{
	bool is_or = binary_expression->base.kind == EXPR_BINARY_LAZY_OR;
	assert(is_or || binary_expression->base.kind == EXPR_BINARY_LAZY_AND);

	dbg_info *dbgi 
		= get_dbg_info(&binary_expression->base.source_position);

	ir_node *val1 = expression_to_firm(binary_expression->left);

	ir_node *cond       = new_d_Cond(dbgi, val1);
	ir_node *true_proj  = new_d_Proj(dbgi, cond, mode_X, pn_Cond_true);
	ir_node *false_proj = new_d_Proj(dbgi, cond, mode_X, pn_Cond_false);

	ir_node *fallthrough_block = new_immBlock();

	/* the true case */
	ir_node *calc_val2_block = new_immBlock();
	if (is_or) {
		add_immBlock_pred(calc_val2_block, false_proj);
	} else {
		add_immBlock_pred(calc_val2_block, true_proj);
	}

	mature_immBlock(calc_val2_block);

	set_cur_block(calc_val2_block);
	ir_node *val2 = expression_to_firm(binary_expression->right);
	if (get_cur_block() != NULL) {
		ir_node *jmp = new_d_Jmp(dbgi);
		add_immBlock_pred(fallthrough_block, jmp);
	}

	/* fallthrough */
	ir_node *constb;
	if (is_or) {
		constb = new_d_Const(dbgi, get_tarval_b_true());
		add_immBlock_pred(fallthrough_block, true_proj);
	} else {
		constb = new_d_Const(dbgi, get_tarval_b_false());
		add_immBlock_pred(fallthrough_block, false_proj);
	}
	mature_immBlock(fallthrough_block);

	set_cur_block(fallthrough_block);

	ir_node *in[2] = { val2, constb };
	ir_node *val   = new_d_Phi(dbgi, 2, in, mode_b);

	return val;
}

static ir_node *binary_expression_to_firm(
		const binary_expression_t *binary_expression)
{
	expression_kind_t kind = binary_expression->base.kind;

	switch (kind) {
	case EXPR_BINARY_ASSIGN:
		return assign_expression_to_firm(binary_expression);
	case EXPR_BINARY_LAZY_OR:
	case EXPR_BINARY_LAZY_AND:
		return create_lazy_op(binary_expression);
	default:
		break;
	}

	ir_node  *left  = expression_to_firm(binary_expression->left);
	ir_node  *right = expression_to_firm(binary_expression->right);
	dbg_info *dbgi
		= get_dbg_info(&binary_expression->base.source_position);


	if (kind == EXPR_BINARY_DIV) {
		ir_mode *mode  = get_ir_mode(binary_expression->base.type);
		ir_node *store = get_store();
		ir_node *node, *res;
		if (mode_is_float(mode)) {
			node  = new_d_Quot(dbgi, store, left, right, mode,
			                   op_pin_state_floats);
			store = new_d_Proj(dbgi, node, mode_M, pn_Quot_M);
			res   = new_d_Proj(dbgi, node, mode, pn_Quot_res);
		} else {
			node = new_d_Div(dbgi, store, left, right, mode,
			                 op_pin_state_floats);
			store = new_d_Proj(dbgi, node, mode_M, pn_Div_M);
			res   = new_d_Proj(dbgi, node, mode, pn_Div_res);
		}
		
		set_store(store);
		return res;
	}

	if (kind == EXPR_BINARY_MOD) {
		ir_mode *mode  = get_ir_mode(binary_expression->base.type);
		ir_node *store = get_store();
		ir_node *node  = new_d_Mod(dbgi, store, left, right, mode,
		                           op_pin_state_floats);
		
		store = new_d_Proj(dbgi, node, mode_M, pn_Mod_M);
		set_store(store);
		return new_d_Proj(dbgi, node, mode, pn_Mod_res);
	}

	/* an arithmetic binexpression? */
	ir_mode *mode = get_ir_mode(binary_expression->base.type);
	switch (kind) {
	case EXPR_BINARY_ADD:
		return new_d_Add(dbgi, left, right, mode);
	case EXPR_BINARY_SUB:
		return new_d_Sub(dbgi, left, right, mode);
	case EXPR_BINARY_MUL:
		return new_d_Mul(dbgi, left, right, mode);
	case EXPR_BINARY_AND:
		return new_d_And(dbgi, left, right, mode);
	case EXPR_BINARY_OR:
		return new_d_Or(dbgi, left, right, mode);
	case EXPR_BINARY_XOR:
		return new_d_Eor(dbgi, left, right, mode);
	case EXPR_BINARY_SHIFTLEFT:
		return new_d_Shl(dbgi, left, right, mode);
	case EXPR_BINARY_SHIFTRIGHT:
		return new_d_Shr(dbgi, left, right, mode);
	default:
		break;
	}

	/* a comparison expression? */
	long compare_pn = binexpr_kind_to_cmp_pn(kind);
	if (compare_pn != 0) {
		ir_node *cmp  = new_d_Cmp(dbgi, left, right);
		ir_node *proj = new_d_Proj(dbgi, cmp, mode_b, compare_pn);

		return proj;
	}

	panic("found unknown binary expression");
}

static ir_node *cast_expression_to_firm(const unary_expression_t *cast)
{
	type_t   *to_type = cast->base.type;
	ir_node  *node    = expression_to_firm(cast->value);
	ir_mode  *mode    = get_ir_mode(to_type);
	dbg_info *dbgi    = get_dbg_info(&cast->base.source_position);

	assert(node != NULL);

	return new_d_Conv(dbgi, node, mode);
}

static ir_node *load_from_expression_addr(type_t *type, ir_node *addr,
                                          const source_position_t *pos)
{
	dbg_info *dbgi  = get_dbg_info(pos);
	ir_mode  *mode  = get_ir_mode(type);
	ir_node  *store = get_store();
	ir_node  *load  = new_d_Load(dbgi, store, addr, mode, cons_none);
	ir_node  *mem   = new_d_Proj(dbgi, load, mode_M, pn_Load_M);
	ir_node  *val   = new_d_Proj(dbgi, load, mode, pn_Load_res);
	set_store(mem);

	return val;
}

typedef ir_node* (*create_unop_node_func) (dbg_info *dbgi, ir_node *value,
                                           ir_mode *mode);

static ir_node *create_unary_expression_node(const unary_expression_t *expression,
                                             create_unop_node_func create_func)
{
	dbg_info *dbgi  = get_dbg_info(&expression->base.source_position);
	type_t   *type  = expression->base.type;
	ir_mode  *mode  = get_ir_mode(type);
	ir_node  *value = expression_to_firm(expression->value);
	ir_node  *res   = create_func(dbgi, value, mode);

	return res;
}

static ir_node *unary_expression_to_firm(
		const unary_expression_t *unary_expression)
{
	ir_node *addr;

	switch (unary_expression->base.kind) {
	case EXPR_UNARY_CAST:
		return cast_expression_to_firm(unary_expression);
	case EXPR_UNARY_DEREFERENCE:
		addr = expression_to_firm(unary_expression->value);
		return load_from_expression_addr(unary_expression->base.type,
		                                 addr,
	                             &unary_expression->base.source_position);
	case EXPR_UNARY_TAKE_ADDRESS:
		return expression_addr(unary_expression->value);
	case EXPR_UNARY_BITWISE_NOT:
	case EXPR_UNARY_NOT:
		return create_unary_expression_node(unary_expression, new_d_Not);
	case EXPR_UNARY_NEGATE:
		return create_unary_expression_node(unary_expression, new_d_Minus);
	case EXPR_UNARY_INCREMENT:
	case EXPR_UNARY_DECREMENT:
		panic("inc/dec expression not lowered");
	default:
		break;
	}
	panic("found unknown unary expression");
}

static ir_node *select_expression_to_firm(const select_expression_t *select)
{
	ir_node *addr       = select_expression_addr(select);
	type_t  *entry_type = select->compound_entry->type;
	if (entry_type->kind == TYPE_COMPOUND_STRUCT	
			|| entry_type->kind == TYPE_COMPOUND_UNION
			|| entry_type->kind == TYPE_ARRAY)
		return addr;

	return load_from_expression_addr(select->base.type, addr,
	                                 &select->base.source_position);
}

static ir_entity *assure_instance(declaration_t *declaration,
                                  type_argument_t *type_arguments)
{
	assert(declaration->kind == DECLARATION_METHOD);
	method_t *method = &declaration->method.method;
	symbol_t *symbol = declaration->base.symbol;

	int old_top        = typevar_binding_stack_top();
	push_type_variable_bindings(method->type_parameters, type_arguments);

	ir_entity  *entity = get_method_entity(method, symbol);
	const char *name   = get_entity_name(entity);

	if (declaration->base.exported 
			&& get_entity_visibility(entity) != visibility_external_allocated) {
		set_entity_visibility(entity, visibility_external_visible);
	}

	pop_type_variable_bindings(old_top);

	if (strset_find(&instantiated_methods, name) != NULL) {
		return entity;
	}

	instantiate_method_t *instantiate 
		= queue_method_instantiation(method, entity);

	type_argument_t *type_argument = type_arguments;
	type_argument_t *last_argument = NULL;
	while (type_argument != NULL) {
		type_t          *type         = type_argument->type;
		type_argument_t *new_argument
			= obstack_alloc(&obst, sizeof(new_argument[0]));
		memset(new_argument, 0, sizeof(new_argument[0]));

		new_argument->type = create_concrete_type(type);

		if (last_argument != NULL) {
			last_argument->next = new_argument;
		} else {
			instantiate->type_arguments = new_argument;
		}
		last_argument = new_argument;

		type_argument = type_argument->next;
	}

	strset_insert(&instantiated_methods, name);

	return entity;
}

static ir_node *method_reference_to_firm(declaration_t *declaration,
                                         type_argument_t *type_arguments,
                                         const source_position_t *source_position)
{
	dbg_info  *dbgi   = get_dbg_info(source_position);
	ir_entity *entity = assure_instance(declaration, type_arguments);

	ir_node *symconst = new_d_SymConst(dbgi, mode_P,
	                                   (union symconst_symbol) entity,
	                                   symconst_addr_ent);

	return symconst;
}

static ir_node *concept_method_reference_to_firm(concept_method_t *method,
                                       type_argument_t *type_arguments,
                                       const source_position_t *source_position)
{
	concept_t *concept = method->concept;

	int old_top = typevar_binding_stack_top();
	push_type_variable_bindings(concept->type_parameters, type_arguments);

	concept_instance_t *instance = find_concept_instance(concept);
	if (instance == NULL) {
		fprintf(stderr, "while looking at method '%s' from '%s'\n",
		        method->base.symbol->string,
		        concept->base.symbol->string);
		print_type(concept->type_parameters->current_type);
		panic("no concept instance found in ast2firm phase");
		return NULL;
	}

	concept_method_instance_t *method_instance 
		= get_method_from_concept_instance(instance, method);
	if (method_instance == NULL) {
		fprintf(stderr, "panic: no method '%s' in instance of concept '%s'\n",
		        method->base.symbol->string,
		        concept->base.symbol->string);
		panic("panic");
		return NULL;
	}

	dbg_info  *dbgi     = get_dbg_info(source_position);
	ir_entity *entity   = get_concept_method_instance_entity(method_instance);
	ir_node   *symconst = new_d_SymConst(dbgi, mode_P,
	                                     (union symconst_symbol) entity,
	                                     symconst_addr_ent);

	pop_type_variable_bindings(old_top);
	return symconst;
}

static ir_node *method_parameter_reference_to_firm(method_parameter_t *parameter)
{
	ir_node *args  = get_irg_args(current_ir_graph);
	ir_mode *mode  = get_ir_mode(parameter->type);
	ir_node *block = get_irg_start_block(current_ir_graph);
	long     pn    = parameter->num;
	ir_node *proj  = new_r_Proj(current_ir_graph, block, args, mode, pn);

	return proj;
}

static ir_node *sizeof_expression_to_firm(const sizeof_expression_t *expression)
{
	ir_mode  *mode = get_ir_mode(expression->base.type);
	unsigned  size = get_type_size(expression->type);
	tarval   *tv   = new_tarval_from_long(size, mode);
	ir_node  *res  = new_Const(tv);

	return res;
}

static ir_node *call_expression_to_firm(const call_expression_t *call)
{
	expression_t  *method = call->method;
	ir_node       *callee = expression_to_firm(method);

	assert(method->base.type->kind == TYPE_POINTER);
	pointer_type_t *pointer_type = (pointer_type_t*) method->base.type;
	type_t         *points_to    = pointer_type->points_to;

	assert(points_to->kind == TYPE_FUNCTION);
	function_type_t *function_type   = (function_type_t*) points_to;
	ir_type         *ir_method_type  = get_ir_type((type_t*) function_type);
	ir_type         *new_method_type = NULL;

	int              n_parameters = 0;
	call_argument_t *argument     = call->arguments;
	while (argument != NULL) {
		n_parameters++;
		argument = argument->next;
	}

	if (function_type->variable_arguments) {
		/* we need to construct a new method type matching the call
		 * arguments... */
		new_method_type = new_type_method(unique_ident("calltype"),
		                                  n_parameters,
		                                  get_method_n_ress(ir_method_type));
		set_method_calling_convention(new_method_type,
		               get_method_calling_convention(ir_method_type));
		set_method_additional_properties(new_method_type,
		               get_method_additional_properties(ir_method_type));

		for (int i = 0; i < get_method_n_ress(ir_method_type); ++i) {
			set_method_res_type(new_method_type, i,
			                    get_method_res_type(ir_method_type, i));
		}
	}
	ir_node *in[n_parameters];

	argument = call->arguments;
	int n = 0;
	while (argument != NULL) {
		expression_t *expression = argument->expression;

		ir_node *arg_node = expression_to_firm(expression);

		in[n] = arg_node;
		if (new_method_type != NULL) {
			ir_type *irtype = get_ir_type(expression->base.type);
			set_method_param_type(new_method_type, n, irtype);
		}

		argument = argument->next;
		n++;
	}

	if (new_method_type != NULL)
		ir_method_type = new_method_type;

	dbg_info *dbgi  = get_dbg_info(&call->base.source_position);
	ir_node  *store = get_store();
	ir_node  *node  = new_d_Call(dbgi, store, callee, n_parameters, in,
	                             ir_method_type);
	ir_node  *mem   = new_d_Proj(dbgi, node, mode_M, pn_Call_M_regular);
	set_store(mem);

	type_t  *result_type = function_type->result_type;
	ir_node *result      = NULL;
	if (result_type->kind != TYPE_VOID) {
		ir_mode *mode    = get_ir_mode(result_type);
		ir_node *resproj = new_d_Proj(dbgi, node, mode_T, pn_Call_T_result);
		result           = new_d_Proj(dbgi, resproj, mode, 0);
	}

	return result;
}

static ir_node *func_expression_to_firm(func_expression_t *expression)
{
	method_t  *method = & expression->method;
	ir_entity *entity = method->e.entity;

	if (entity == NULL) {
		symbol_t *symbol = unique_symbol("anonfunc");
		entity           = get_method_entity(method, symbol);
	}
	queue_method_instantiation(method, entity);

	ir_node *symconst = new_SymConst(mode_P, (union symconst_symbol) entity,
	                                 symconst_addr_ent);

	return symconst;
}

static ir_node *declaration_reference_to_firm(declaration_t *declaration,
                                       type_argument_t *type_arguments,
									   const source_position_t *source_position)
{
	method_declaration_t *method_declaration;

	switch (declaration->kind) {
	case DECLARATION_METHOD:
		method_declaration = (method_declaration_t*) declaration;
		return method_reference_to_firm(declaration, type_arguments,
		                                source_position);
	case DECLARATION_ITERATOR:
		// TODO
		panic("TODO: iterator to firm");
		break;
	case DECLARATION_CONCEPT_METHOD:
		return concept_method_reference_to_firm(
				(concept_method_t*) declaration, type_arguments,
		        source_position);
	case DECLARATION_METHOD_PARAMETER:
		return method_parameter_reference_to_firm(
				(method_parameter_t*) declaration);
	case DECLARATION_CONSTANT:
		return constant_reference_to_firm((constant_t*) declaration);
	case DECLARATION_VARIABLE:
		return variable_to_firm((variable_declaration_t*) declaration,
		                        source_position);
	case DECLARATION_INVALID:
	case DECLARATION_ERROR:
	case DECLARATION_TYPEALIAS:
	case DECLARATION_CONCEPT:
	case DECLARATION_LABEL:
	case DECLARATION_TYPE_VARIABLE:
		panic("internal error: trying to construct node for non-data "
		      "reference");
	}
	panic("unknown declaration type found");
}

static ir_node *reference_expression_to_firm(const reference_expression_t *reference)
{
	declaration_t   *declaration    = reference->declaration;
	type_argument_t *type_arguments = reference->type_arguments;
	return declaration_reference_to_firm(declaration, type_arguments,
	                                    &reference->base.source_position);
}

static ir_node *expression_to_firm(expression_t *expression)
{
	ir_node *addr;

	switch (expression->kind) {
	case EXPR_INT_CONST:
		return int_const_to_firm(&expression->int_const);
	case EXPR_FLOAT_CONST:
		return float_const_to_firm(&expression->float_const);
	case EXPR_STRING_CONST:
		return string_const_to_firm(&expression->string_const);
	case EXPR_BOOL_CONST:
		return bool_const_to_firm(&expression->bool_const);
	case EXPR_NULL_POINTER:
		return null_pointer_to_firm();
	case EXPR_REFERENCE:
		return reference_expression_to_firm(&expression->reference);
	EXPR_BINARY_CASES
		return binary_expression_to_firm(&expression->binary);
	EXPR_UNARY_CASES
		return unary_expression_to_firm(&expression->unary);
	case EXPR_SELECT:
		return select_expression_to_firm(&expression->select);
	case EXPR_ARRAY_ACCESS:
		addr = expression_addr(expression);
		return load_from_expression_addr(expression->base.type, addr,
		                                 &expression->base.source_position);
	case EXPR_CALL:
		return call_expression_to_firm(&expression->call);
	case EXPR_SIZEOF:
		return sizeof_expression_to_firm(&expression->sizeofe);
	case EXPR_FUNC:
		return func_expression_to_firm(&expression->func);
	case EXPR_INVALID:
	case EXPR_ERROR:
		break;
	}
	abort();
	return NULL;
}


static void statement_to_firm(statement_t *statement);

static void return_statement_to_firm(const return_statement_t *statement)
{
	dbg_info *dbgi = get_dbg_info(&statement->base.source_position);
	ir_node *ret;
	if (statement->value != NULL) {
		ir_node *retval = expression_to_firm(statement->value);
		ir_node *in[1];

		in[0] = retval;
		ret = new_d_Return(dbgi, get_store(), 1, in);
	} else {
		ret = new_d_Return(dbgi, get_store(), 0, NULL);
	}
	ir_node *end_block = get_irg_end_block(current_ir_graph);
	add_immBlock_pred(end_block, ret);

	set_cur_block(NULL);
}

static void if_statement_to_firm(const if_statement_t *statement)
{
	dbg_info *dbgi      = get_dbg_info(&statement->base.source_position);
	ir_node  *condition = expression_to_firm(statement->condition);
	assert(condition != NULL);

	ir_node *cond       = new_d_Cond(dbgi, condition);
	ir_node *true_proj  = new_d_Proj(dbgi, cond, mode_X, pn_Cond_true);
	ir_node *false_proj = new_d_Proj(dbgi, cond, mode_X, pn_Cond_false);

	ir_node *fallthrough_block = new_immBlock();

	/* the true (blocks) */
	ir_node *true_block = new_immBlock();
	add_immBlock_pred(true_block, true_proj);
	mature_immBlock(true_block);

	set_cur_block(true_block);
	statement_to_firm(statement->true_statement);
	if (get_cur_block() != NULL) {
		ir_node *jmp = new_Jmp();
		add_immBlock_pred(fallthrough_block, jmp);
	}

	/* the false (blocks) */
	if (statement->false_statement != NULL) {
		ir_node *false_block = new_immBlock();
		add_immBlock_pred(false_block, false_proj);
		mature_immBlock(false_block);

		set_cur_block(false_block);
		statement_to_firm(statement->false_statement);
		if (get_cur_block() != NULL) {
			ir_node *jmp = new_Jmp();
			add_immBlock_pred(fallthrough_block, jmp);
		}
	} else {
		add_immBlock_pred(fallthrough_block, false_proj);
	}
	mature_immBlock(fallthrough_block);

	set_cur_block(fallthrough_block);
}

static void expression_statement_to_firm(const expression_statement_t *statement)
{
	expression_to_firm(statement->expression);
}

static void block_statement_to_firm(const block_statement_t *block)
{
	context2firm(&block->context);

	statement_t *statement = block->statements;
	for ( ; statement != NULL; statement = statement->base.next) {
		statement_to_firm(statement);
	}
}

static void goto_statement_to_firm(goto_statement_t *goto_statement)
{
	dbg_info            *dbgi  
		= get_dbg_info(&goto_statement->base.source_position);
	label_declaration_t *label = goto_statement->label;
	ir_node             *block = label->block;

	if (block == NULL) {
		block        = new_immBlock();
		label->block = block;
		label->next  = labels;
		labels       = label;
	}
	ir_node *jmp = new_d_Jmp(dbgi);
	add_immBlock_pred(block, jmp);

	set_cur_block(NULL);
}

static void label_statement_to_firm(label_statement_t *label_statement)
{
	label_declaration_t *label = &label_statement->declaration;
	ir_node *block = label->block;

	if (block == NULL) {
		block         = new_immBlock();
		label->block  = block;
		label->next   = labels;
		labels        = label;
	}

	if (get_cur_block() != NULL) {
		ir_node *jmp = new_Jmp();

		add_immBlock_pred(block, jmp);
	}
	set_cur_block(block);
}

static void statement_to_firm(statement_t *statement)
{
	if (statement->kind != STATEMENT_LABEL && get_cur_block() == NULL) {
		fprintf(stderr, "Warning: unreachable code detected\n");
		return;
	}

	switch (statement->kind) {
	case STATEMENT_BLOCK:
		block_statement_to_firm(&statement->block);
		return;
	case STATEMENT_RETURN:
		return_statement_to_firm(&statement->returns);
		return;
	case STATEMENT_IF:
		if_statement_to_firm(&statement->ifs);
		return;
	case STATEMENT_DECLARATION:
		/* nothing to do */
		return;
	case STATEMENT_EXPRESSION:
		expression_statement_to_firm(&statement->expression);
		return;
	case STATEMENT_LABEL:
		label_statement_to_firm(&statement->label);
		return;
	case STATEMENT_GOTO:
		goto_statement_to_firm(&statement->gotos);
		return;
	case STATEMENT_ERROR:
	case STATEMENT_INVALID:
		break;
	}
	panic("Invalid statement kind found");
}

static void create_method(method_t *method, ir_entity *entity,
                          type_argument_t *type_arguments)
{
	if (method->is_extern)
		return;

	int old_top = typevar_binding_stack_top();
	if (is_polymorphic_method(method)) {
		assert(type_arguments != NULL);
		push_type_variable_bindings(method->type_parameters, type_arguments);
	}

	ir_graph *irg     = new_ir_graph(entity, method->n_local_vars);

	assert(variable_context == NULL);
	variable_context = get_irg_frame(irg);

	assert(value_numbers == NULL);
	value_numbers = xmalloc(method->n_local_vars * sizeof(value_numbers[0]));

	context2firm(&method->context);

	ir_node *firstblock = get_cur_block();

	if (method->statement)
		statement_to_firm(method->statement);

	/* no return statement seen yet? */
	ir_node *end_block = get_irg_end_block(irg);
	if (get_cur_block() != NULL) {
		ir_node *ret = new_Return(get_store(), 0, NULL);
		add_immBlock_pred(end_block, ret);
	}

	mature_immBlock(firstblock);
	mature_immBlock(end_block);

	label_declaration_t *label = labels;
	while (label != NULL) {
		mature_immBlock(label->block);
		label->block = NULL;
		label        = label->next;
	}
	labels = NULL;

	irg_finalize_cons(irg);

	/* finalize the frame type */
	ir_type *frame_type = get_irg_frame_type(irg);
	int      n          = get_compound_n_members(frame_type);
	int      align_all  = 4;
	int      offset     = 0;
	for (int i = 0; i < n; ++i) {
		ir_entity *entity      = get_compound_member(frame_type, i);
		ir_type   *entity_type = get_entity_type(entity);
	
		int align = get_type_alignment_bytes(entity_type);
		if (align > align_all)
			align_all = align;
		int misalign = 0;
		if (align > 0) {
			misalign  = offset % align;
			offset   += misalign;
		}

		set_entity_offset(entity, offset);
		offset += get_type_size_bytes(entity_type);
	}
	set_type_size_bytes(frame_type, offset);
	set_type_alignment_bytes(frame_type, align_all);
	set_type_state(frame_type, layout_fixed);

	irg_vrfy(irg);

	free(value_numbers);
	value_numbers = NULL;

	variable_context = NULL;

	pop_type_variable_bindings(old_top);
}

static void create_concept_instance(concept_instance_t *instance)
{
	if (instance->type_parameters != NULL)
		return;

	concept_method_instance_t *method_instance = instance->method_instances;
	for ( ; method_instance != NULL; method_instance = method_instance->next) {
		/* we have to construct this instance lazily
		   TODO: construct all instances lazily might be a good idea */
		method_t *method = & method_instance->method;
		/* make sure the method entity is set */
		ir_entity *entity = get_concept_method_instance_entity(method_instance);
		/* we can emit it like a normal method */
		queue_method_instantiation(method, entity);
	}
}

static void context2firm(const context_t *context)
{
	/* scan context for functions */
	declaration_t *declaration = context->declarations;
	for ( ; declaration != NULL; declaration = declaration->base.next) {
		switch (declaration->kind) {
		case DECLARATION_METHOD:
			if (!is_polymorphic_method(&declaration->method.method)) {
				assure_instance(declaration, NULL);
			}

			break;
		case DECLARATION_VARIABLE:
			create_variable_entity((variable_declaration_t*) declaration);
			break;
		case DECLARATION_ITERATOR:
		case DECLARATION_TYPEALIAS:
		case DECLARATION_CONCEPT:
		case DECLARATION_CONSTANT:
		case DECLARATION_LABEL:
		case DECLARATION_METHOD_PARAMETER:
		case DECLARATION_CONCEPT_METHOD:
		case DECLARATION_TYPE_VARIABLE:
			break;
		case DECLARATION_INVALID:
		case DECLARATION_ERROR:
			panic("Invalid namespace entry type found");
		}
	}

	/* TODO: create these always lazily? */
	concept_instance_t *instance = context->concept_instances;
	for ( ; instance != NULL; instance = instance->next) {
		create_concept_instance(instance);
	}
}

/**
 * Build a firm representation of the program
 */
void ast2firm(const module_t *modules)
{
	obstack_init(&obst);
	strset_init(&instantiated_methods);
	instantiate_methods = new_pdeq();

	init_ir_types();

	assert(typevar_binding_stack_top() == 0);

	/* transform toplevel stuff */
	const module_t *module = modules;
	for ( ; module != NULL; module = module->next) {
		context2firm(&module->context);
	}

	/* work generic code instantiation queue */
	while (!pdeq_empty(instantiate_methods)) {
		instantiate_method_t *instantiate_method 
			= pdeq_getl(instantiate_methods);

		assert(typevar_binding_stack_top() == 0);

		create_method(instantiate_method->method,
		              instantiate_method->entity,
		              instantiate_method->type_arguments);
	}

	assert(typevar_binding_stack_top() == 0);

	del_pdeq(instantiate_methods);
	obstack_free(&obst, NULL);
	strset_destroy(&instantiated_methods);
}

