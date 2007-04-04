#include <config.h>

#include <assert.h>
#include <firm/common/firm.h>
#include <firm/be/be.h>

#include "ast_t.h"
#include "semantic.h"
#include "adt/error.h"
#include "adt/xmalloc.h"

static variable_declaration_statement_t **value_numbers = NULL;

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
}

void exit_firm(void)
{
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
	default:
		panic("Encountered unknown atomic type");
	}
}

static
ir_type *get_atomic_type(const atomic_type_t *type)
{
	ir_mode *mode = get_atomic_mode(type);
	ident *id = get_mode_ident(mode);
	ir_type *irtype = new_type_primitive(id, mode);

	return irtype;
}

static
ir_type *get_ir_type(type_t *type)
{
	assert(type != NULL);

	if(type->firm_type != NULL) {
		return type->firm_type;
	}

	ir_type *firm_type = NULL;
	switch(type->type) {
	case TYPE_ATOMIC:
		firm_type = get_atomic_type((const atomic_type_t*) type);
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
	/* TODO FIXME remove me later */
	if(type == NULL)
		return mode_Is;

	ir_type *irtype = get_ir_type(type);
	ir_mode *mode = get_type_mode(irtype);
	assert(mode != NULL);
	return mode;
}

static
ir_node *expression_to_firm(const expression_t *expression);

static
ir_node *int_const_to_firm(const int_const_t *cnst)
{
	ir_mode *mode = get_ir_mode(cnst->expression.datatype);
	tarval *tv = new_tarval_from_long(cnst->value, mode);

	return new_Const(mode, tv);
}

static
ir_node *variable_reference_to_firm(const variable_reference_expression_t *ref)
{
	variable_declaration_statement_t *variable = ref->variable;
	ir_mode                          *mode     = get_ir_mode(variable->type);

	value_numbers[variable->value_number] = variable;
	
	return get_value(variable->value_number, mode);
}

static
ir_node *asssign_expression_to_firm(const assign_expression_t *assign)
{
	const expression_t *left  = assign->left;
	const expression_t *right = assign->right;

	assert(left->type == EXPR_VARIABLE_REFERENCE);
	const variable_reference_expression_t *ref 
		= (const variable_reference_expression_t*) left;
	variable_declaration_statement_t *variable = ref->variable;

	value_numbers[variable->value_number] = variable;

	ir_node *val = expression_to_firm(right);
	
	set_value(variable->value_number, val);

	return val;
}

static
ir_node *expression_to_firm(const expression_t *expression)
{
	switch(expression->type) {
	case EXPR_INT_CONST:
		return int_const_to_firm((const int_const_t*) expression);
	case EXPR_VARIABLE_REFERENCE:
		return variable_reference_to_firm(
				(const variable_reference_expression_t*) expression);
	case EXPR_ASSIGN:
		return asssign_expression_to_firm(
				(const assign_expression_t*) expression);
	default:
		assert(0);
	}
	return NULL;
}

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
}

static
void expression_statement_to_firm(const expression_statement_t *statement)
{
	expression_to_firm(statement->expression);
}

static
void statement_to_firm(const statement_t *statement);

static
void block_statement_to_firm(const block_statement_t *block)
{
	const statement_t *statement = block->first_statement;
	while(statement != NULL) {
		statement_to_firm(statement);
		statement = statement->next;
	}
}

static
void statement_to_firm(const statement_t *statement)
{
	switch(statement->type) {
	case STATEMENT_BLOCK:
		block_statement_to_firm((const block_statement_t*) statement);
		return;
	case STATEMENT_RETURN:
		return_statement_to_firm((const return_statement_t*) statement);
		return;
	case STATEMENT_VARIABLE_DECLARATION:
		/* nothing to do */
		break;
	case STATEMENT_EXPRESSION:
		expression_statement_to_firm((const expression_statement_t*) statement);
		break;
	default:
		assert(0);
	}
}

static
void create_function(const function_t *function)
{
	/* first create an entity */
	ir_type *glob_type = get_glob_type();
	ident *id = new_id_from_str(function->symbol->string);
	ir_type *methodtype = new_type_method(id, 0, 1);
	set_method_res_type(methodtype, 0, get_ir_type(function->return_type));
	/*TODO
	 * set_method_res_type(methodtype, 0, int_type); */
	ir_entity *entity = new_entity(glob_type, id, methodtype);
	set_entity_ld_ident(entity, id);
	set_entity_visibility(entity, visibility_external_visible);

	ir_graph *irg = new_ir_graph(entity, function->n_local_vars);

	assert(value_numbers == NULL);
	value_numbers = xmalloc(function->n_local_vars * sizeof(value_numbers[0]));

	statement_to_firm(function->statement);

	mature_immBlock(get_irg_current_block(irg));
	mature_immBlock(get_irg_end_block(irg));

	irg_finalize_cons(irg);

	irg_vrfy(irg);

	free(value_numbers);
	value_numbers = NULL;

	dump_ir_block_graph(irg, "-test");
}

/**
 * Build a firm representation of an AST programm
 */
void ast2firm(namespace_t *namespace)
{
	/* scan compilation unit for functions */
	namespace_entry_t *entry = namespace->first_entry;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_FUNCTION:
			create_function((function_t*) entry);
			break;
		case NAMESPACE_ENTRY_VARIABLE:
			fprintf(stderr, "Global vars not handled yet\n");
			break;
		default:
			panic("Unknown namespace entry type found");
		}

		entry = entry->next;
	}
}

