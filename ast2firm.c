#include <config.h>

#include <assert.h>
#include <firm/common/firm.h>
#include <firm/be/be.h>

#include "ast_t.h"
#include "adt/error.h"

static
ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos)
{
	(void) irg;
	(void) pos;
	/* TODO generate warning */
	return new_Unknown(mode);
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
ir_mode *get_atomic_int_mode(const atomic_type_t* atomic_type)
{
	switch(atomic_type->bits) {
	case 8:
		return atomic_type->has_sign ? mode_Bs : mode_Bu;
	case 16:
		return atomic_type->has_sign ? mode_Hs : mode_Hu;
	case 32:
		return atomic_type->has_sign ? mode_Is : mode_Iu;
	case 64:
		return atomic_type->has_sign ? mode_Ls : mode_Lu;
	default:
		panic("invalid atomic type");
	}
	return NULL;
}

static
ir_type *get_atomic_type(const atomic_type_t* atomic_type)
{
	switch(atomic_type->atype) {
	case ATOMIC_TYPE_INT:
	}
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
ir_node *int_const_to_firm(const int_const_t *cnst)
{
	ir_mode *mode = get_ir_mode(cnst->expression.datatype);
	tarval *tv = new_tarval_from_long(cnst->value, mode);

	return new_Const(mode, tv);
}

static
ir_node *expression_to_firm(const expression_t *expression)
{
	switch(expression->type) {
	case EXPR_INT_CONST:
		return int_const_to_firm((int_const_t*) expression);
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
	default:
		assert(0);
	}
}

/**
 * Build a firm representation of an AST programm
 */
void ast2firm(compilation_unit_t *unit)
{
	/* scan compilation unit for functions */
	environment_entry_t *entry = unit->environment.entries;
	for( ; entry != NULL; entry = entry->next) {
		if(entry->type == ENTRY_FUNCTION) {
			function_t *func = entry->function;
			/* first create an entity */
			ir_type *glob_type = get_glob_type();
			ident *id = new_id_from_str(entry->symbol->string);
			ir_type *methodtype = new_type_method(id, 0, 1);
			set_method_res_type(methodtype, 0, get_ir_type(func->return_type));
			/*TODO
			 * set_method_res_type(methodtype, 0, int_type); */
			ir_entity *entity = new_entity(glob_type, id, methodtype);
			set_entity_ld_ident(entity, id);
			set_entity_visibility(entity, visibility_external_visible);

			ir_graph *irg = new_ir_graph(entity, 0);

			statement_to_firm(func->statement);

			mature_immBlock(get_irg_current_block(irg));
			mature_immBlock(get_irg_end_block(irg));

			irg_finalize_cons(irg);

			irg_vrfy(irg);

			dump_ir_block_graph(irg, "-test");
		}
	}
}

