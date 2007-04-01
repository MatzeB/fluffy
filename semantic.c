#include <config.h>

#include "semantic.h"
#include "ast_t.h"
#include "adt/error.h"

static atomic_type_t default_int_type_ 
	= { { TYPE_ATOMIC, NULL }, ATOMIC_TYPE_INT };
static type_t *default_int_type = (type_t*) &default_int_type_;

static
void check_expression(expression_t *expression)
{
	switch(expression->type) {
	case EXPR_INT_CONST:
		expression->datatype = default_int_type;
		break;
	case EXPR_CAST:
		if(expression->datatype == NULL) {
			panic("Cast expression needs a datatype!");
		}
		break;
	default:
		panic("Invalid expression encountered");
	}
}

static
void check_return_statement(return_statement_t *statement)
{
	if(statement->return_value != NULL) {
		check_expression(statement->return_value);

#if 0
		ir_type *func_return_type = /* TODO */
		if(statement->return_value->datatype != func_return_type) {
			/* test if cast is possible... */

			cast_expression_t *cast = obstack_alloc(&env->obst, sizeof(cast[0]);
			memset(cast, 0, sizeof(cast[0]));
			cast->expression.type = EXPR_CAST;
			cast->expression.datatype = func_return_type;
			cast->value = statement->return_value);
			statement->return_value = cast;
		}
#endif
	}
}

static
void check_statement(statement_t *statement);

static
void check_block_statement(block_statement_t *block)
{
	statement_t *statement = block->first_statement;
	while(statement != NULL) {
		check_statement(statement);
		statement = statement->next;
	}
}

static
void check_statement(statement_t *statement)
{
	switch(statement->type) {
	case STATEMENT_INVALID:
		panic("encountered invalid statement");
	case STATEMENT_BLOCK:
		check_block_statement((block_statement_t*) statement);
		break;
	case STATEMENT_RETURN:
		check_return_statement((return_statement_t*) statement);
		break;
	}
}

static
void check_function(function_t *function)
{
	/* resolve type refs... */
	
	check_statement(function->statement);
}

void check_static_semantic(compilation_unit_t *unit)
{
	environment_entry_t *entry = unit->environment.entries;
	while(entry != NULL) {
		switch(entry->type) {
		case ENTRY_FUNCTION:
			check_function(entry->function);
			break;
		default:
			break;
		}
		
		entry = entry->next;
	}
}
