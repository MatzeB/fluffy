#include <config.h>

#define _GNU_SOURCE

#include <assert.h>
#include <string.h>
#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "ast_t.h"
#include "semantic.h"
#include "mangle_type.h"
#include "adt/obst.h"
#include "adt/error.h"
#include "adt/xmalloc.h"

static variable_declaration_statement_t **value_numbers = NULL;
static label_statement_t                 *labels        = NULL;

static ir_type *byte_array_type = NULL;

typedef struct instantiate_method_t  instantiate_method_t;

struct instantiate_method_t {
	method_t             *method;
	type_argument_t      *type_arguments;
	ir_entity            *entity;

	instantiate_method_t *next;
};

static struct obstack obst;
static instantiate_method_t *instantiate_methods = NULL;

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

	ir_type *byte_ir_type = get_ir_type((type_t*) &byte_type);
	byte_array_type       = new_type_array(new_id_from_str("bytearray"), 1,
	                                       byte_ir_type);
	set_array_lower_bound_int(byte_array_type, 0, 0);
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
ir_type *get_atomic_type(const atomic_type_t *type)
{
	ir_mode *mode   = get_atomic_mode(type);
	ident *id       = get_mode_ident(mode);
	ir_type *irtype = new_type_primitive(id, mode);

	return irtype;
}

static
unsigned get_type_size(const type_t *type);

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
unsigned get_struct_type_size(const struct_type_t *type)
{
	(void) type;
	panic("sizeof struct type not implemented yet");
}

static
unsigned get_type_reference_type_var_size(const type_reference_t *type)
{
	type_variable_t *type_variable = type->r.type_variable;

	if(type_variable->current_type == NULL) {
		panic("Taking size of unbound type variable");
		return 0;
	}
	return get_type_size(type_variable->current_type);
}

static
unsigned get_type_size(const type_t *type)
{
	switch(type->type) {
	case TYPE_VOID:
		return 0;
	case TYPE_ATOMIC:
		return get_atomic_type_size((const atomic_type_t*) type);
	case TYPE_STRUCT:
		return get_struct_type_size((const struct_type_t*) type);
	case TYPE_METHOD:
		panic("It's not possible to determine the size of a method type");
		break;
	case TYPE_POINTER:
		return 4;
	case TYPE_REFERENCE:
		panic("Type reference not resolved");
		break;
	case TYPE_REFERENCE_TYPE:
		return get_type_size( ((const type_reference_t*) type)->r.type );
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
ir_type *get_struct_as_pointer_type(type_t *type)
{
	ir_type *ir_type = get_ir_type(type);

	if(type->type == TYPE_STRUCT) {
		ident *id = unique_id("struct_ptr");
		ir_type = new_type_pointer(id, ir_type, mode_P_data);
	}
	return ir_type;
}

static
ir_type *get_method_type(const method_type_t *method_type)
{
	type_t  *result_type  = method_type->result_type;

	ident   *id           = unique_id("methodtype");
	int      n_parameters = count_parameters(method_type);
	int      n_results    = result_type->type == TYPE_VOID ? 0 : 1;
	ir_type *irtype       = new_type_method(id, n_parameters, n_results);

	if(result_type->type != TYPE_VOID) {
		ir_type *restype = get_struct_as_pointer_type(result_type);
		set_method_res_type(irtype, 0, restype);
	}

	method_parameter_type_t *param_type = method_type->parameter_types;
	int n = 0;
	while(param_type != NULL) {
		ir_type *p_irtype = get_struct_as_pointer_type(param_type->type);
		set_method_param_type(irtype, n, p_irtype);

		param_type = param_type->next;
		n++;
	}

	return irtype;
}

static
ir_type *get_pointer_type(pointer_type_t *type)
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
	type->type.firm_type = ir_type;

	ir_points_to = get_ir_type(points_to);
	set_pointer_points_to_type(ir_type, ir_points_to);

	return ir_type;
}

#define INVALID_TYPE ((ir_type_ptr)-1)

static
ir_type *get_struct_type(struct_type_t *type)
{
	ir_type *ir_type = new_type_struct(unique_id(type->symbol->string));
#ifndef NDEBUG
	/* detect structs that contain themselfes. The semantic phase should have
	 * already reported errors about this but you never know... */
	type->type.firm_type = INVALID_TYPE;
#endif

	int align_all = 1;
	int offset    = 0;
	struct_entry_t *entry = type->entries;
	while(entry != NULL) {
		ident       *ident         = new_id_from_str(entry->symbol->string);
		ir_type_ptr  entry_ir_type = get_struct_as_pointer_type(entry->type);

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
ir_type *get_ir_type(type_t *type)
{
	assert(type != NULL);

	if(type->firm_type != NULL) {
		assert(type->firm_type != INVALID_TYPE);
		return type->firm_type;
	}

	ir_type *firm_type = NULL;
	switch(type->type) {
	case TYPE_ATOMIC:
		firm_type = get_atomic_type((atomic_type_t*) type);
		break;
	case TYPE_METHOD:
		firm_type = get_method_type((method_type_t*) type);
		break;
	case TYPE_POINTER:
		firm_type = get_pointer_type((pointer_type_t*) type);
		break;
	case TYPE_VOID:
		/* there is no mode_VOID in firm, use mode_C */
		firm_type = new_type_primitive(new_id_from_str("void"), mode_C);
		break;
	case TYPE_STRUCT:
		firm_type = get_struct_type((struct_type_t*) type);
		break;
	default:
		panic("unknown type");
	}

	type->firm_type = firm_type;
	return firm_type;
}

static inline
ir_mode *get_ir_mode(type_t *type)
{
	if(type->type == TYPE_STRUCT) {
		return mode_P_data;
	}
	ir_type *irtype = get_ir_type(type);
	ir_mode *mode   = get_type_mode(irtype);
	assert(mode != NULL);
	return mode;
}

static
instantiate_method_t *queue_method_instantiation(method_t *method)
{
	instantiate_method_t *instantiate
		= obstack_alloc(&obst, sizeof(instantiate[0]));
	memset(instantiate, 0, sizeof(instantiate[0]));

	instantiate->method = method;
	instantiate->next   = instantiate_methods;
	instantiate_methods = instantiate;

	return instantiate;
}

static
int is_polymorphic_method(const method_t *method)
{
	return method->type_parameters != NULL;
}

static
ir_entity* get_typeclass_method_instance_entity(
		typeclass_method_instance_t *method_instance)
{
	method_t *method = method_instance->method;
	if(method->entity != NULL)
		return method->entity;

	method_type_t *method_type = method->type;
	if(method_type->abi_style != NULL) {
		fprintf(stderr, "Warning: ABI Style '%s' unknown\n",
		        method_type->abi_style);
	}

	typeclass_method_t *typeclass_method = method_instance->typeclass_method;
	typeclass_t        *typeclass        = typeclass_method->typeclass;
	const char         *string           = typeclass->symbol->string;
	size_t              len              = strlen(string);
	obstack_printf(&obst, "_tcv%zu%s", len, string);

	string = typeclass_method->symbol->string;
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

	fprintf(stderr, "Mangled Name: '%s'\n", str);

	ident *id = new_id_from_str(str);
	obstack_free(&obst, str);

	/* create the entity */
	ir_type *global_type    = get_glob_type();
	ir_type *ir_method_type = get_ir_type((type_t*) method->type);

	ir_entity *entity       = new_entity(global_type, id, ir_method_type);
	set_entity_ld_ident(entity, id);
	set_entity_visibility(entity, visibility_external_visible);

	method->entity = entity;
	return entity;
}

static
ir_entity* get_method_entity(method_t *method, type_argument_t *type_arguments)
{
	if(method->entity != NULL)
		return method->entity;

	method_type_t *method_type = method->type;

	if(method_type->abi_style != NULL) {
		fprintf(stderr, "Warning: ABI Style '%s' unknown\n",
		        method_type->abi_style);
	}

	ident *id;
	if(type_arguments == NULL) {
		id = new_id_from_str(method->symbol->string);
	} else {
		const char *string = method->symbol->string;
		size_t      len    = strlen(string);
		obstack_printf(&obst, "_v%zu%s", len, string);

		type_argument_t *argument = type_arguments;
		while(argument != NULL) {
			mangle_type(&obst, argument->type);
			argument = argument->next;
		}
		obstack_1grow(&obst, 0);

		char *str = obstack_finish(&obst);

		fprintf(stderr, "Mangled Name: '%s'\n", str);

		id = new_id_from_str(str);
		obstack_free(&obst, str);
	}

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
ir_entity* get_extern_method_entity(extern_method_t *method)
{
	if(method->entity != NULL)
		return method->entity;

	method_type_t *method_type = method->type;

	if(method_type->abi_style != NULL) {
		fprintf(stderr, "Warning: ABI Style '%s' unknown\n",
		        method_type->abi_style);
	}

	/* first create an entity */
	ir_type *global_type    = get_glob_type();
	ident   *id             = new_id_from_str(method->symbol->string);
	ir_type *ir_method_type = get_ir_type((type_t*) method_type);

	ir_entity *entity       = new_entity(global_type, id, ir_method_type);
	set_entity_ld_ident(entity, id);
	set_entity_visibility(entity, visibility_external_allocated);

	method->entity = entity;
	return entity;
}

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
	ir_entity *ent         = new_entity(global_type, unique_id("str"),
	                                    byte_array_type);

	ir_type    *elem_type  = get_array_element_type(byte_array_type);
	ir_mode    *mode       = get_type_mode(elem_type);

	const char *string     = cnst->value;
	size_t      slen       = strlen(string) + 1;
	tarval    **tvs        = xmalloc(slen * sizeof(tvs[0]));
	for(size_t i = 0; i < slen; ++i) {
		tvs[i] = new_tarval_from_long(string[i], mode);
	}

	set_entity_variability(ent, variability_constant);
	set_array_entity_values(ent, tvs, slen);
	free(tvs);

	return new_SymConst((union symconst_symbol) ent, symconst_addr_ent);
}

static
ir_node *variable_reference_to_firm(const reference_expression_t *ref)
{
	variable_declaration_statement_t *variable = ref->r.variable;
	ir_mode                          *mode     = get_ir_mode(variable->type);

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
	ir_entity    *entity            = select->struct_entry->entity;
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
ir_node *expression_addr(const expression_t *expression)
{
	const unary_expression_t *unexpr;

	switch(expression->type) {
	case EXPR_SELECT:
		return select_expression_addr((const select_expression_t*) expression);
	case EXPR_ARRAY_ACCESS:
		return array_access_expression_addr(
				(const array_access_expression_t*) expression);
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

	if(left->type == EXPR_REFERENCE_VARIABLE) {
		const reference_expression_t *ref
			= (const reference_expression_t*) left;
		variable_declaration_statement_t *variable = ref->r.variable;

		value_numbers[variable->value_number] = variable;
		set_value(variable->value_number, value);
	} else {
		ir_node *addr       = expression_addr(left);
		ir_node *store      = get_store();
		ir_node *store_node = new_Store(store, addr, value);
		ir_node *mem        = new_Proj(store_node, mode_M, pn_Store_M);
		set_store(mem);
	}

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
		return pn_Cmp_Leg;
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
	ir_mode *mode      = get_ir_mode(cast->expression.datatype);
	ir_node *conv_node = new_Conv(node, mode);

	return conv_node;
}

static
ir_node *load_from_expression_addr(const expression_t *expression,
                                   ir_node *addr)
{
	ir_mode *mode  = get_ir_mode(expression->datatype);
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
		return load_from_expression_addr(&unary_expression->expression, addr);
	default:
		abort();
	}
}

static
ir_node *method_reference_to_firm(const reference_expression_t *ref)
{
	method_t  *method = ref->r.method;
	ir_entity *entity = get_method_entity(method, ref->type_arguments);

	if(is_polymorphic_method(method)) {
		instantiate_method_t *instantiate =
			queue_method_instantiation(ref->r.method);
		instantiate->type_arguments = ref->type_arguments;
		instantiate->entity         = entity;
	}

	ir_node *symconst = new_SymConst((union symconst_symbol) entity,
	                                 symconst_addr_ent);

	return symconst;
}

static
ir_node *typeclass_method_instance_reference_to_firm(
		const reference_expression_t *ref)
{
	typeclass_method_instance_t *method_instance 
		= ref->r.typeclass_method_instance;

	ir_entity *entity   = get_typeclass_method_instance_entity(method_instance);
	ir_node   *symconst	= new_SymConst((union symconst_symbol) entity,
	                                   symconst_addr_ent);
	return symconst;
}

static
ir_node *extern_method_reference_to_firm(const reference_expression_t *ref)
{
	ir_entity *entity = get_extern_method_entity(ref->r.extern_method);
	ir_node *symconst = new_SymConst((union symconst_symbol) entity,
	                                 symconst_addr_ent);

	return symconst;
}

static
ir_node *method_parameter_reference_to_firm(const reference_expression_t *ref)
{
	ir_node *args  = get_irg_args(current_ir_graph);
	ir_mode *mode  = get_ir_mode(ref->r.method_parameter->type);
	ir_node *block = get_irg_start_block(current_ir_graph);
	long     pn    = ref->r.method_parameter->num;
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
	ir_node       *result         = NULL;
	expression_t  *method         = call->method;
	assert(method->datatype->type == TYPE_METHOD);
	method_type_t *method_type    = (method_type_t*) method->datatype;
	ir_type       *ir_method_type = get_ir_type((type_t*) method_type);

	ir_node       *callee         = expression_to_firm(method);
	int            n_parameters   = get_method_n_params(ir_method_type);
	ir_node       *in[n_parameters];

	call_argument_t *argument     = call->arguments;
	int n = 0;
	while(argument != NULL) {
		assert(n < n_parameters);
		in[n] = expression_to_firm(argument->expression);

		argument = argument->next;
		n++;
	}
	assert(n == n_parameters);

	ir_node *store = get_store();
	ir_node *node  = new_Call(store, callee, n_parameters, in, ir_method_type);
	ir_node *mem   = new_Proj(node, mode_M, pn_Call_M_regular);
	set_store(mem);

	type_t *result_type = method_type->result_type;
	if(result_type->type != TYPE_VOID) {
		ir_mode *mode    = get_ir_mode(result_type);
		ir_node *resproj = new_Proj(node, mode_T, pn_Call_T_result);
		result           = new_Proj(resproj, mode, 0);
	}

	return result;
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
	case EXPR_REFERENCE_VARIABLE:
		return variable_reference_to_firm(
				(const reference_expression_t*) expression);
	case EXPR_REFERENCE_METHOD:
		return method_reference_to_firm(
		        (const reference_expression_t*) expression);
	case EXPR_REFERENCE_EXTERN_METHOD:
		return extern_method_reference_to_firm(
		        (const reference_expression_t*) expression);
	case EXPR_REFERENCE_METHOD_PARAMETER:
		return method_parameter_reference_to_firm(
				(const reference_expression_t*) expression);
	case EXPR_REFERENCE_TYPECLASS_METHOD_INSTANCE:
		return typeclass_method_instance_reference_to_firm(
				(const reference_expression_t*) expression);
	case EXPR_BINARY:
		return binary_expression_to_firm(
				(const binary_expression_t*) expression);
	case EXPR_UNARY:
		return unary_expression_to_firm(
				(const unary_expression_t*) expression);
	case EXPR_ARRAY_ACCESS:
	case EXPR_SELECT:
		addr = expression_addr(expression);
		return load_from_expression_addr(expression, addr);
	case EXPR_CALL:
		return call_expression_to_firm((const call_expression_t*) expression);
	case EXPR_SIZEOF:
		return sizeof_expression_to_firm(
				(const sizeof_expression_t*) expression);
	case EXPR_REFERENCE:
		panic("reference expressions not lowered");
	case EXPR_REFERENCE_GLOBAL_VARIABLE:
		panic("global variable references not handled yet");
	case EXPR_REFERENCE_TYPECLASS_METHOD:
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
	statement_t *statement = block->first_statement;
	while(statement != NULL) {
		statement_to_firm(statement);
		statement = statement->next;
	}
}

static
void goto_statement_to_firm(goto_statement_t *goto_statement)
{
	label_statement_t *label = goto_statement->label;
	ir_node           *block = label->block;

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
void label_statement_to_firm(label_statement_t *label)
{
	ir_node *block = label->block;

	if(block == NULL) {
		ir_node *temp = get_cur_block();
		block        = new_immBlock();
		set_cur_block(temp);
		label->block = block;
		label->next  = labels;
		labels       = label;
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
void create_method(method_t *method, type_argument_t *type_arguments)
{
	if(is_polymorphic_method(method)) {
		if(type_arguments == NULL)
			return;
		
		type_argument_t *type_argument = type_arguments;
		type_variable_t *type_variable = method->type_parameters;
		while(type_argument != NULL) {
			if(type_variable == NULL) {
				panic("type arguments and type variables don't match");
				return;
			}

			type_variable->current_type = type_argument->type;

			type_argument = type_argument->next;
			type_variable = type_variable->next;
		}
		if(type_variable != NULL) {
			panic("type arguments and type variables don't match");
			return;
		}
	}
	
	ir_entity *entity = get_method_entity(method, NULL);

	ir_graph *irg = new_ir_graph(entity, method->n_local_vars);

	assert(value_numbers == NULL);
	value_numbers = xmalloc(method->n_local_vars * sizeof(value_numbers[0]));

	ir_node *firstblock = get_cur_block();

	if(method->statement)
		statement_to_firm(method->statement);

	/* no return statement seen yet? */
	ir_node *end_block     = get_irg_end_block(irg);
	if(get_cur_block() != NULL) {
		ir_node *ret = new_Return(get_store(), 0, NULL);
		add_immBlock_pred(end_block, ret);
	}

	mature_immBlock(firstblock);
	mature_immBlock(end_block);

	label_statement_t *label = labels;
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

	dump_ir_block_graph(irg, "-test");
}

static
void create_typeclass_instance(typeclass_instance_t *instance)
{
	typeclass_method_instance_t *method_instance = instance->method_instances;
	while(method_instance != NULL) {
		method_t *method  = method_instance->method;
		/* make sure the method entity is set */
		get_typeclass_method_instance_entity(method_instance);
		/* we can emit it like a normal method */
		create_method(method, NULL);

		method_instance  = method_instance->next;
	}
}

/**
 * Build a firm representation of an AST programm
 */
void ast2firm(namespace_t *namespace)
{
	obstack_init(&obst);

	/* scan compilation unit for functions */
	namespace_entry_t *entry = namespace->first_entry;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_METHOD:
			create_method((method_t*) entry, NULL);
			break;
		case NAMESPACE_ENTRY_TYPECLASS_INSTANCE:
			create_typeclass_instance((typeclass_instance_t*) entry);
			break;
		case NAMESPACE_ENTRY_VARIABLE:
			fprintf(stderr, "Global vars not handled yet\n");
			break;
		case NAMESPACE_ENTRY_STRUCT:
		case NAMESPACE_ENTRY_TYPECLASS:
		case NAMESPACE_ENTRY_EXTERN_METHOD:
			break;
		default:
			panic("Unknown namespace entry type found");
		}

		entry = entry->next;
	}

	instantiate_method_t *instantiate_method = instantiate_methods;
	while(instantiate_method != NULL) {
		create_method(instantiate_method->method,
		              instantiate_method->type_arguments);

		instantiate_method = instantiate_method->next;
	}

	obstack_free(&obst, NULL);
}

