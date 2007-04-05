#include <config.h>

#include "semantic.h"

#include "ast_t.h"
#include "type_hash.h"
#include "adt/obst.h"
#include "adt/array.h"
#include "adt/error.h"

typedef enum   environment_entry_type_t environment_entry_type_t;
typedef struct semantic_env_t           semantic_env_t;

enum environment_entry_type_t {
	ENTRY_LOCAL_VARIABLE,
	ENTRY_GLOBAL_VARIABLE,
	ENTRY_METHOD_PARAMETER,
	ENTRY_METHOD,
	ENTRY_EXTERN_METHOD
};

struct environment_entry_t {
	environment_entry_type_t  type;
	symbol_t                 *symbol;
	environment_entry_t      *up;
	union {
		method_t                         *method;
		variable_t                       *global_variable;
		method_parameter_t               *method_parameter;
		extern_method_t                  *extern_method;
		variable_declaration_statement_t *variable;
	};
};

struct semantic_env_t {
	struct obstack       *obst;
	struct obstack        symbol_obstack;
	environment_entry_t **symbol_stack;
	int                   next_valnum;
	int                   found_errors;

	method_t             *current_method;
	type_t               *type_bool;
	type_t               *type_int;
};

/**
 * pushs an environment_entry on the environment stack and links the
 * corresponding symbol to the new entry
 */
static inline
environment_entry_t *environment_push(semantic_env_t *env, symbol_t *symbol)
{
	environment_entry_t *entry
		= obstack_alloc(&env->symbol_obstack, sizeof(entry[0]));
	memset(entry, 0, sizeof(entry[0]));

	int top = ARR_LEN(env->symbol_stack);
	ARR_RESIZE(environment_entry_t*, env->symbol_stack, top + 1);
	env->symbol_stack[top] = entry;

	printf("Push %s\n", symbol->string);

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
	environment_entry_t *entry = NULL;
	size_t top = ARR_LEN(env->symbol_stack);
	size_t i;

	if(new_top == top)
		return;

	assert(new_top < top);
	i = top;
	do {
		          entry  = env->symbol_stack[i - 1];
		symbol_t *symbol = entry->symbol;

		printf("Pop %s\n", symbol->string);

		if(entry->type == ENTRY_LOCAL_VARIABLE
				&& entry->variable->refs == 0) {
			fprintf(stderr, "Warning: Variable '%s' was declared but never read\n", symbol->string);
		}

		assert(symbol->thing == entry);
		symbol->thing = entry->up;

		--i;
	} while(i != new_top);
	obstack_free(&env->symbol_obstack, entry);

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

static
void check_expression(semantic_env_t *env, expression_t *expression);

static
void check_reference_expression(semantic_env_t *env,
                                reference_expression_t *ref)
{
	variable_declaration_statement_t *variable;
	method_t                         *method;
	method_parameter_t               *method_parameter;
	extern_method_t                  *extern_method;
	variable_t                       *global_variable;
	symbol_t                         *symbol = ref->symbol;
	environment_entry_t              *entry  = symbol->thing;

	if(entry == NULL) {
		fprintf(stderr, "Error: No known definition for '%s'\n", symbol->string);
		env->found_errors = 1;
		return;
	}

	switch(entry->type) {
	case ENTRY_LOCAL_VARIABLE:
		variable                 = entry->variable;
		ref->variable            = variable;
		ref->expression.type     = EXPR_REFERENCE_VARIABLE;
		ref->expression.datatype = variable->type;
		variable->refs++;
		break;
	case ENTRY_METHOD:
		method                   = entry->method;
		ref->method              = method;
		ref->expression.type     = EXPR_REFERENCE_METHOD;
		ref->expression.datatype = (type_t*) method->type;
		break;
	case ENTRY_EXTERN_METHOD:
		extern_method            = entry->extern_method;
		ref->extern_method       = extern_method;
		ref->expression.type     = EXPR_REFERENCE_EXTERN_METHOD;
		ref->expression.datatype = (type_t*) extern_method->type;
		break;
	case ENTRY_GLOBAL_VARIABLE:
		global_variable          = entry->global_variable;
		ref->global_variable     = global_variable;
		ref->expression.type     = EXPR_REFERENCE_GLOBAL_VARIABLE;
		ref->expression.datatype = global_variable->type;
		break;
	case ENTRY_METHOD_PARAMETER:
		method_parameter         = entry->method_parameter;
		ref->method_parameter    = method_parameter;
		ref->expression.type     = EXPR_REFERENCE_METHOD_PARAMETER;
		ref->expression.datatype = method_parameter->type;
		break;
	default:
		panic("Unknown reference type encountered");
		break;
	}
}

static
void check_assign_expression(semantic_env_t *env, binary_expression_t *assign)
{
	expression_t *left  = assign->left;

	if(left->type != EXPR_REFERENCE_VARIABLE) {
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
	reference_expression_t *ref = (reference_expression_t*) left;
	ref->variable->refs--;

	assign->expression.datatype = left->datatype;
}

static
expression_t *make_cast(semantic_env_t *env, expression_t *from,
                        type_t *destination_type)
{
	assert(from->datatype != destination_type);

	cast_expression_t *cast = obstack_alloc(env->obst, sizeof(cast[0]));
	memset(cast, 0, sizeof(cast[0]));
	cast->expression.type     = EXPR_CAST;
	cast->expression.datatype = destination_type;
	cast->value               = from;

	return (expression_t*) cast;
}

static
int is_arithmetic_op(binexpr_type_t type)
{
	switch(type) {
	case BINEXPR_ADD:
	case BINEXPR_SUB:
	case BINEXPR_MUL:
	case BINEXPR_DIV:
	case BINEXPR_SHIFTLEFT:
	case BINEXPR_SHIFTRIGHT:
		return 1;
	default:
		return 0;
	}
}

static
int is_comparison_op(binexpr_type_t type)
{
	switch(type) {
	case BINEXPR_EQUAL:
	case BINEXPR_NOTEQUAL:
	case BINEXPR_LESS:
	case BINEXPR_LESSEQUAL:
	case BINEXPR_GREATER:
	case BINEXPR_GREATEREQUAL:
		return 1;
	default:
		return 0;
	}
}

static
void check_binary_expression(semantic_env_t *env, binary_expression_t *binexpr)
{
	expression_t *left  = binexpr->left;
	expression_t *right = binexpr->right;

	check_expression(env, left);
	check_expression(env, right);

	type_t *exprtype;
	type_t *lefttype, *righttype;
	binexpr_type_t binexpr_type = binexpr->binexpr_type;

	if(binexpr_type == BINEXPR_ASSIGN) {
		check_assign_expression(env, binexpr);
		exprtype  = left->datatype;
		lefttype  = exprtype;
		righttype = exprtype;
	} else if(is_arithmetic_op(binexpr_type)) {
		exprtype  = left->datatype;
		/* TODO find out greatest common type... */
		lefttype  = exprtype;
		righttype = exprtype;
	} else if(is_comparison_op(binexpr_type)) {
		exprtype  = env->type_bool;
		/* TODO find out greatest common type... */
		lefttype  = left->datatype;
		righttype = left->datatype;
	} else {
		abort();
	}

	if(left->datatype != lefttype) {
		binexpr->left  = make_cast(env, left, exprtype);
	}
	if(right->datatype != righttype) {
		binexpr->right = make_cast(env, right, exprtype);
	}
	binexpr->expression.datatype = exprtype;
}

static
void check_call_expression(semantic_env_t *env, call_expression_t *call)
{
	expression_t  *method = call->method;

	check_expression(env, method);
	type_t *type          = method->datatype;

	/* can happen if we had a deeper semantic error */
	if(type == NULL)
		return;

	if(type->type != TYPE_METHOD) {
		fprintf(stderr, "Trying to call something which is not a method\n");
		env->found_errors = 1;
		return;
	}

	method_type_t *method_type = (method_type_t*) type;
	call->expression.datatype  = method_type->result_type;

	call_argument_t *argument           = call->arguments;
	method_parameter_type_t *param_type = method_type->parameter_types;
	while(argument != NULL) {
		if(param_type == NULL) {
			fprintf(stderr, "Too few arguments for method call\n");
			env->found_errors = 1;
			break;
		}

		expression_t *expression  = argument->expression;
		type_t       *wanted_type = param_type->type;
		check_expression(env, expression);
		if(expression->datatype != wanted_type) {
			expression = make_cast(env, expression, wanted_type);
		}
		argument->expression = expression;

		argument   = argument->next;
		param_type = param_type->next;
	}
	if(param_type != NULL) {
		fprintf(stderr, "Too much argumentss for method call\n");
		env->found_errors = 1;
	}
}

static
void check_cast_expression(semantic_env_t *env, cast_expression_t *cast)
{
	if(cast->expression.datatype == NULL) {
		panic("Cast expression needs a datatype!");
	}

	check_expression(env, cast->value);
}

static
void check_expression(semantic_env_t *env, expression_t *expression)
{
	switch(expression->type) {
	case EXPR_INT_CONST:
		expression->datatype = env->type_int;
		break;
	case EXPR_CAST:
		check_cast_expression(env, (cast_expression_t*) expression);
		break;
	case EXPR_REFERENCE:
		check_reference_expression(env, (reference_expression_t*) expression);
		break;
	case EXPR_BINARY:
		check_binary_expression(env, (binary_expression_t*) expression);
		break;
	case EXPR_CALL:
		check_call_expression(env, (call_expression_t*) expression);
		break;
	default:
		panic("Invalid expression encountered");
	}
}




static
void check_statement(semantic_env_t *env, statement_t *statement);

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
void check_if_statement(semantic_env_t *env, if_statement_t *statement)
{
	expression_t *condition = statement->condition;

	check_expression(env, condition);
	if(condition->datatype != env->type_bool) {
		fprintf(stderr, "Error: if condition needs to be of boolean type\n");
		env->found_errors = 1;
		return;
	}

	check_statement(env, statement->true_statement);
	if(statement->false_statement != NULL)
		check_statement(env, statement->false_statement);
}

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
	environment_entry_t *entry = environment_push(env, statement->symbol);
	entry->type                = ENTRY_LOCAL_VARIABLE;
	entry->variable            = statement;

	if(env->current_method != NULL) {
		env->current_method->n_local_vars++;
	}
}

static
void check_expression_statement(semantic_env_t *env,
                                expression_statement_t *statement)
{
	expression_t *expression = statement->expression;

	check_expression(env, expression);

	/* can happen on semantic errors */
	if(expression->datatype == NULL)
		return;

	int is_assign = 0;
	if(expression->type == EXPR_BINARY &&
			((binary_expression_t*) expression)->binexpr_type == BINEXPR_ASSIGN)
		is_assign = 1;

	if(expression->datatype != void_type && !is_assign) {
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
	case STATEMENT_IF:
		check_if_statement(env, (if_statement_t*) statement);
		break;
	case STATEMENT_VARIABLE_DECLARATION:
		check_variable_declaration(env, (variable_declaration_statement_t*)
		                                statement);
		break;
	case STATEMENT_EXPRESSION:
		check_expression_statement(env, (expression_statement_t*) statement);
		break;
	default:
		panic("Unknown statement found");
		break;
	}
}

static
void check_method(semantic_env_t *env, method_t *method)
{
	int old_top         = environment_top(env);
	env->current_method = method;

	/* push method parameters */
	method_parameter_t *parameter = method->parameters;
	int n = 0;
	while(parameter != NULL) {
		environment_entry_t *entry = environment_push(env, parameter->symbol);
		entry->type                = ENTRY_METHOD_PARAMETER;
		entry->method_parameter    = parameter;
		parameter->num             = n;
		n++;

		parameter = parameter->next;
	}

	check_statement(env, method->statement);

	env->current_method = NULL;
	environment_pop_to(env, old_top);
}

static
void check_namespace(semantic_env_t *env, namespace_t *namespace)
{
	variable_t          *variable;
	method_t            *method;
	extern_method_t     *extern_method;
	environment_entry_t *env_entry;
	int old_top         = environment_top(env);

	/* record namespace entries in environment */
	namespace_entry_t *entry = namespace->first_entry;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_VARIABLE:
			variable            = (variable_t*) entry;
			env_entry           = environment_push(env, variable->symbol);
			env_entry->type     = ENTRY_GLOBAL_VARIABLE;
			env_entry->global_variable = variable;
			break;
		case NAMESPACE_ENTRY_EXTERN_METHOD:
			extern_method       = (extern_method_t*) entry;
			env_entry           = environment_push(env, extern_method->symbol);
			env_entry->type     = ENTRY_EXTERN_METHOD;
			env_entry->extern_method = extern_method;
			break;
		case NAMESPACE_ENTRY_METHOD:
			method              = (method_t*) entry;
			env_entry           = environment_push(env, method->symbol);
			env_entry->type     = ENTRY_METHOD;
			env_entry->method   = method;
			break;
		default:
			panic("Unknown thing in namespace");
			break;
		}
		entry = entry->next;
	}

	/* check semantics in methods */
	entry = namespace->first_entry;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_METHOD:
			check_method(env, (method_t*) entry);
			break;
		default:
			break;
		}

		entry = entry->next;
	}

	environment_pop_to(env, old_top);
}

static
type_t* make_atomic_type(struct obstack *obst, atomic_type_type_t atype)
{
	atomic_type_t *type = obstack_alloc(obst, sizeof(type[0]));
	memset(type, 0, sizeof(type[0]));
	type->type.type = TYPE_ATOMIC;
	type->atype     = atype;

	type_t *normalized_type = typehash_insert((type_t*) type);
	if(normalized_type != (type_t*) type) {
		obstack_free(obst, type);
	}

	return normalized_type;
}

int check_static_semantic(namespace_t *namespace)
{
	struct obstack obst;
	semantic_env_t env;

	obstack_init(&obst);
	env.obst         = &obst;
	env.symbol_stack = NEW_ARR_F(environment_entry_t*, 0);
	obstack_init(&env.symbol_obstack);
	env.next_valnum  = 0;
	env.found_errors = 0;
	env.type_bool    = make_atomic_type(&obst, ATOMIC_TYPE_BOOL);
	env.type_int     = make_atomic_type(&obst, ATOMIC_TYPE_INT);

	check_namespace(&env, namespace);

	DEL_ARR_F(env.symbol_stack);

	// TODO global obstack...
	//obstack_free(&obst, NULL);
	obstack_free(&env.symbol_obstack, NULL);

	return !env.found_errors;
}
