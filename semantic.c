#include <config.h>

#include "semantic.h"

#include "ast_t.h"
#include "adt/obst.h"
#include "adt/array.h"
#include "adt/error.h"

typedef enum   environment_entry_type_t environment_entry_type_t;
typedef struct semantic_env_t           semantic_env_t;

enum environment_entry_type_t {
	ENTRY_LOCAL_VARIABLE,
	ENTRY_GLOBAL_VARIABLE,
	ENTRY_FUNCTION,
	ENTRY_EXTERN_FUNCTION
};

struct environment_entry_t {
	environment_entry_type_t  type;
	symbol_t                 *symbol;
	environment_entry_t      *up;
	union {
		function_t                       *function;
		variable_t                       *variable;
		extern_function_t                *extern_function;
		variable_declaration_statement_t *variable_declaration;
	};
};

struct semantic_env_t {
	struct obstack       *obst;
	environment_entry_t  *symbol_stack;
	int                   next_valnum;
	int                   found_errors;

	function_t           *current_function;
};

/**
 * pushs an environment_entry on the environment stack and links the
 * corresponding symbol to the new entry
 */
static inline
environment_entry_t *environment_push(semantic_env_t *env, symbol_t *symbol)
{
	int top = ARR_LEN(env->symbol_stack);
	ARR_RESIZE(environment_entry_t, env->symbol_stack, top + 1);
	environment_entry_t *entry = & env->symbol_stack[top];

	entry->up     = symbol->thing;
	entry->symbol = symbol;
	symbol->thing = entry;

	return entry;
}

/**
 * pops symbols from the environment stack until @p new_top is the top element
 */
static inline
void environment_pop_to(semantic_env_t *env, size_t new_top)
{
	size_t top = ARR_LEN(env->symbol_stack);
	size_t i;

	if(new_top == top)
		return;

	assert(new_top < top);
	i = top;
	do {
		environment_entry_t *entry  = & env->symbol_stack[i - 1];
		symbol_t            *symbol = entry->symbol;

		if(entry->type == ENTRY_LOCAL_VARIABLE
				&& entry->variable_declaration->refs == 0) {
			fprintf(stderr, "Warning: Variable '%s' was declared but never read\n", symbol->string);
		}

		assert(symbol->thing == entry);
		symbol->thing = entry->up;

		--i;
	} while(i != new_top);

	ARR_SHRINKLEN(env->symbol_stack, (int) new_top);
}

/**
 * returns the top element of the environment stack
 */
static inline
size_t environment_top(semantic_env_t *env)
{
	return ARR_LEN(env->symbol_stack);
}

static atomic_type_t default_int_type_ 
	= { { TYPE_ATOMIC, NULL }, ATOMIC_TYPE_INT };
static type_t *default_int_type = (type_t*) &default_int_type_;

static
void check_expression(semantic_env_t *env, expression_t *expression);

static
void check_variable_reference_expression(semantic_env_t *env,
                                         variable_reference_expression_t *ref)
{
	variable_declaration_statement_t *variable;
	symbol_t            *symbol = ref->symbol;
	environment_entry_t *entry  = symbol->thing;
	
	if(entry == NULL) {
		fprintf(stderr, "Error: No known definition for '%s'\n", symbol->string);
		env->found_errors = 1;
		return;
	}
	if(entry->type != ENTRY_LOCAL_VARIABLE) {
		fprintf(stderr,
		        "NIY: Only local variable references supported at the moment '%s'\n",
				symbol->string);
		env->found_errors = 1;
		return;
	}
	variable                 = entry->variable_declaration;
	ref->variable            = variable;
	ref->expression.datatype = variable->type;
	variable->refs++;
}

static
void check_assign_expression(semantic_env_t *env, assign_expression_t *assign)
{
	expression_t *left  = assign->left;
	expression_t *right = assign->right;

	check_expression(env, left);
	check_expression(env, right);

	if(left->type != EXPR_VARIABLE_REFERENCE) {
		fprintf(stderr, "Error: Left side of assign is not an lvalue.\n");
		env->found_errors = 1;
		return;
	}
	if(left->datatype->type != TYPE_ATOMIC) {
		fprintf(stderr, "NIY: Only primitive types in assignments supported at the moment\n");
		env->found_errors = 1;
		return;
	}

	/* assignment is not reading the value */
	variable_reference_expression_t *ref
		= (variable_reference_expression_t*) left;
	ref->variable->refs--;

	/* TODO 
	if(left->datatype != right->datatype) {
		// need a cast on right side...
	}
	*/
	assign->expression.datatype = left->datatype;
}

static
void check_expression(semantic_env_t *env, expression_t *expression)
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
	case EXPR_VARIABLE_REFERENCE:
		check_variable_reference_expression(env,
		        (variable_reference_expression_t*) expression);
		break;
	case EXPR_ASSIGN:
		check_assign_expression(env, (assign_expression_t*) expression);
		break;
	default:
		panic("Invalid expression encountered");
	}
}

static
void check_return_statement(semantic_env_t *env, return_statement_t *statement)
{
	if(statement->return_value != NULL) {
		check_expression(env, statement->return_value);

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
void check_statement(semantic_env_t *env, statement_t *statement);

static
void check_block_statement(semantic_env_t *env, block_statement_t *block)
{
	int old_top = environment_top(env);

	statement_t *statement = block->first_statement;
	while(statement != NULL) {
		check_statement(env, statement);
		statement = statement->next;
	}

	environment_pop_to(env, old_top);
}

static
void check_variable_declaration(semantic_env_t *env,
                                variable_declaration_statement_t *statement)
{
	statement->value_number = env->next_valnum;
	statement->refs         = 0;
	env->next_valnum++;

	/* push the variable declaration on the environment stack */
	environment_entry_t *entry  = environment_push(env, statement->symbol);
	entry->type                 = ENTRY_LOCAL_VARIABLE;
	entry->variable_declaration = statement;

	if(env->current_function != NULL) {
		env->current_function->n_local_vars++;
	}
}

static
void check_expression_statement(semantic_env_t *env,
                                expression_statement_t *statement)
{
	expression_t *expression = statement->expression;

	check_expression(env, expression);
	if(expression->type != EXPR_ASSIGN
			&& expression->datatype->type != TYPE_VOID) {
		fprintf(stderr, "Warning: result of expression is unused\n");
		fprintf(stderr, "Note: Cast expression to void to avoid this warning\n");
	}
}

static
void check_statement(semantic_env_t *env, statement_t *statement)
{
	switch(statement->type) {
	case STATEMENT_INVALID:
		panic("encountered invalid statement");
	case STATEMENT_BLOCK:
		check_block_statement(env, (block_statement_t*) statement);
		break;
	case STATEMENT_RETURN:
		check_return_statement(env, (return_statement_t*) statement);
		break;
	case STATEMENT_VARIABLE_DECLARATION:
		check_variable_declaration(env, (variable_declaration_statement_t*)
		                                statement);
		break;
	case STATEMENT_EXPRESSION:
		check_expression_statement(env, (expression_statement_t*) statement);
		break;
	case STATEMENT_IF:
		panic("envountered unimplemented statement");
		break;
	}
}

static
void check_function(semantic_env_t *env, function_t *function)
{
	int old_top           = environment_top(env);
	env->current_function = function;

	check_statement(env, function->statement);

	env->current_function = NULL;
	environment_pop_to(env, old_top);
}

static
void check_namespace(semantic_env_t *env, namespace_t *namespace)
{
	variable_t          *variable;
	function_t          *function;
	extern_function_t   *extern_function;
	environment_entry_t *env_entry;

	/* record namespace entries in environment */
	namespace_entry_t *entry = namespace->first_entry;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_VARIABLE:
			variable            = (variable_t*) entry;
			env_entry           = environment_push(env, variable->symbol);
			env_entry->type     = ENTRY_GLOBAL_VARIABLE;
			env_entry->variable = variable;
			break;
		case NAMESPACE_ENTRY_EXTERN_FUNCTION:
			extern_function = (extern_function_t*) entry;
			env_entry       = environment_push(env, extern_function->symbol);
			env_entry->type = ENTRY_EXTERN_FUNCTION;
			env_entry->extern_function = extern_function;
			break;
		case NAMESPACE_ENTRY_FUNCTION:
			function            = (function_t*) entry;
			env_entry           = environment_push(env, function->symbol);
			env_entry->type     = ENTRY_FUNCTION;
			env_entry->function = function;
			break;
		default:
			panic("Unknown thing in namespace");
			break;
		}
		entry = entry->next;
	}

	/* check semantics in functions */
	entry = namespace->first_entry;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_FUNCTION:
			check_function(env, (function_t*) entry);
			break;
		default:
			break;
		}
		
		entry = entry->next;
	}
}

int check_static_semantic(namespace_t *namespace)
{
	struct obstack obst;
	semantic_env_t env;

	obstack_init(&obst);
	env.obst         = &obst;
	env.symbol_stack = NEW_ARR_F(environment_entry_t, 0);
	env.next_valnum  = 0;
	env.found_errors = 0;

	check_namespace(&env, namespace);

	DEL_ARR_F(env.symbol_stack);

	// TODO global obstack...
	//obstack_free(&obst, NULL);

	return !env.found_errors;
}

