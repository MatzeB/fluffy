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
	ENTRY_TYPE,
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
		type_t                           *stored_type;
		variable_declaration_statement_t *variable;
	} e;
};

struct semantic_env_t {
	struct obstack       *obst;
	struct obstack        symbol_obstack;
	environment_entry_t **symbol_stack;
	int                   next_valnum;
	int                   found_errors;

	method_t             *current_method;
	int                   last_statement_was_return;
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

		if(entry->type == ENTRY_LOCAL_VARIABLE
				&& entry->e.variable->refs == 0) {
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
type_t *normalize_type(semantic_env_t *env, type_t *type);

static
type_t *normalize_ref_type(semantic_env_t *env, ref_type_t *type_ref)
{
	environment_entry_t *entry    = type_ref->symbol->thing;
	if(entry == NULL) {
		fprintf(stderr, "Error: Can't resolve type: Symbol '%s' is "
				"unknown\n", type_ref->symbol->string);
		env->found_errors = 1;
		return NULL;
	}
	if(entry->type != ENTRY_TYPE) {
		fprintf(stderr, "Error: Symbol '%s' is not a type\n",
				type_ref->symbol->string);
		env->found_errors = 1;
		return NULL;
	}

	return entry->e.stored_type;
}

static
type_t *normalize_pointer_type(semantic_env_t *env, pointer_type_t *type)
{
	type->points_to = normalize_type(env, type->points_to);

	return typehash_insert((type_t*) type);
}

static
type_t *normalize_method_type(semantic_env_t *env, method_type_t *method_type)
{
	method_type->result_type = normalize_type(env, method_type->result_type);

	method_parameter_type_t *parameter = method_type->parameter_types;
	while(parameter != NULL) {
		parameter->type = normalize_type(env, parameter->type);

		parameter = parameter->next;
	}

	return typehash_insert((type_t*) method_type);
}

static
type_t *normalize_struct_type(semantic_env_t *env, struct_type_t *struct_type)
{
	struct_entry_t *entry = struct_type->entries;
	while(entry != NULL) {
		entry->type = normalize_type(env, entry->type);

		entry = entry->next;
	}

	return typehash_insert((type_t*) struct_type);
}

static
type_t *normalize_type(semantic_env_t *env, type_t *type)
{
	switch(type->type) {
	case TYPE_INVALID:
	case TYPE_VOID:
	case TYPE_ATOMIC:
		return type;

	case TYPE_REF:
		return normalize_ref_type(env, (ref_type_t*) type);

	case TYPE_POINTER:
		return normalize_pointer_type(env, (pointer_type_t*) type);
	
	case TYPE_METHOD:
		return normalize_method_type(env, (method_type_t*) type);

	case TYPE_STRUCT:
		return normalize_struct_type(env, (struct_type_t*) type);

	default:
		panic("Unknown type found");
	}
}

static
void check_expression(semantic_env_t *env, expression_t *expression);

static
void check_local_variable_type(semantic_env_t *env,
                               variable_declaration_statement_t *declaration,
                               type_t *type)
{
	if(type->type != TYPE_ATOMIC && type->type != TYPE_POINTER
			&& type->type != TYPE_STRUCT) {
		fprintf(stderr, "Only atomic or pointer types allowed for local "
		        "variables (at variable '%s')\n", declaration->symbol->string);
		env->found_errors = 1;
	}
}

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
		variable                 = entry->e.variable;
		ref->r.variable          = variable;
		ref->expression.type     = EXPR_REFERENCE_VARIABLE;
		ref->expression.datatype = variable->type;
		variable->refs++;
		break;
	case ENTRY_METHOD:
		method                   = entry->e.method;
		ref->r.method            = method;
		ref->expression.type     = EXPR_REFERENCE_METHOD;
		ref->expression.datatype = (type_t*) method->type;
		break;
	case ENTRY_EXTERN_METHOD:
		extern_method            = entry->e.extern_method;
		ref->r.extern_method     = extern_method;
		ref->expression.type     = EXPR_REFERENCE_EXTERN_METHOD;
		ref->expression.datatype = (type_t*) extern_method->type;
		break;
	case ENTRY_GLOBAL_VARIABLE:
		global_variable          = entry->e.global_variable;
		ref->r.global_variable   = global_variable;
		ref->expression.type     = EXPR_REFERENCE_GLOBAL_VARIABLE;
		ref->expression.datatype = global_variable->type;
		break;
	case ENTRY_METHOD_PARAMETER:
		method_parameter         = entry->e.method_parameter;
		ref->r.method_parameter  = method_parameter;
		ref->expression.type     = EXPR_REFERENCE_METHOD_PARAMETER;
		ref->expression.datatype = method_parameter->type;
		break;
	case ENTRY_TYPE:
		fprintf(stderr, "Error: Can't use type in expression\n");
		env->found_errors = 1;
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
	expression_t *right = assign->right;

	if(left->type != EXPR_REFERENCE_VARIABLE &&
			left->type != EXPR_SELECT) {
		fprintf(stderr, "Error: Left side of assign is not an lvalue.\n");
		env->found_errors = 1;
		return;
	}
	if(left->type == EXPR_REFERENCE_VARIABLE) {
		reference_expression_t *ref = (reference_expression_t*) left;
		variable_declaration_statement_t *variable = ref->r.variable;
		symbol_t                         *symbol   = variable->symbol;

		/* do type inferencing if needed */
		if(left->datatype == NULL) {
			if(right->datatype == NULL) {
				fprintf(stderr, "Error: Can't infer type for '%s'\n",
				        symbol->string);
				env->found_errors = 1;
				return;
			}

			variable->type = right->datatype;
			left->datatype = right->datatype;
			fprintf(stderr, "Type inference for '%s': ", symbol->string);
			print_type(stderr, right->datatype);
			fputs("\n", stderr);

			check_local_variable_type(env, variable, right->datatype);
		}

		/* making an assignment is not reading the value */
		variable->refs--;
	}
}

static
expression_t *make_cast(semantic_env_t *env, expression_t *from,
                        type_t *destination_type)
{
	assert(from->datatype != destination_type);

	unary_expression_t *cast = obstack_alloc(env->obst, sizeof(cast[0]));
	memset(cast, 0, sizeof(cast[0]));
	cast->expression.type     = EXPR_UNARY;
	cast->type                = UNEXPR_CAST;
	cast->expression.datatype = destination_type;
	cast->value               = from;

	return (expression_t*) cast;
}

static
int is_arithmetic_op(binary_expression_type_t type)
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
int is_comparison_op(binary_expression_type_t type)
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
	binary_expression_type_t binexpr_type = binexpr->type;

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
void check_cast_expression(semantic_env_t *env, unary_expression_t *cast)
{
	if(cast->expression.datatype == NULL) {
		panic("Cast expression needs a datatype!");
	}
	cast->expression.datatype = normalize_type(env, cast->expression.datatype);

	check_expression(env, cast->value);
}

static
void check_dereference_expression(semantic_env_t *env,
                                  unary_expression_t *dereference)
{
	expression_t *value = dereference->value;
	check_expression(env, value);
	if(value->datatype == NULL) {
		fprintf(stderr,
		        "Error: can't derefence expression with unknown datatype\n");
		env->found_errors = 1;
		return;
	}
	if(value->datatype->type != TYPE_POINTER) {
		fprintf(stderr,
		        "Can only dereference expressions with pointer type\n");
		env->found_errors = 1;
		return;
	}
	pointer_type_t *pointer_type      = (pointer_type_t*) value->datatype;
	type_t         *dereferenced_type = pointer_type->points_to;
	dereference->expression.datatype  = dereferenced_type;
}

static
void check_unary_expression(semantic_env_t *env,
                            unary_expression_t *unary_expression)
{
	switch(unary_expression->type) {
	case UNEXPR_CAST:
		check_cast_expression(env, unary_expression);
		break;
	case UNEXPR_DEREFERENCE:
		check_dereference_expression(env, unary_expression);
		break;
	default:
		abort();
		break;
	}
}

static
void check_select_expression(semantic_env_t *env, select_expression_t *select)
{
	expression_t *compound = select->compound;
	check_expression(env, compound);

	type_t *compound_type = compound->datatype;
	if(compound_type->type != TYPE_STRUCT) {
		fprintf(stderr, "Compound type in select expression is not a pointer "
		                "to a struct\n");
		env->found_errors = 1;
		return;
	}

	struct_type_t  *struct_type   = (struct_type_t*) compound_type;
	struct_entry_t *entry         = struct_type->entries;
	symbol_t       *symbol        = select->symbol;
	while(entry != NULL) {
		if(entry->symbol == symbol) {
			break;
		}
		entry = entry->next;
	}
	if(entry == NULL) {
		fprintf(stderr, "Struct type '%s' does not contain a member '%s'\n",
		        struct_type->symbol->string, symbol->string);
		env->found_errors = 1;
		return;
	}
	select->struct_entry        = entry;
	select->expression.datatype = entry->type;
}

static
void check_expression(semantic_env_t *env, expression_t *expression)
{
	switch(expression->type) {
	case EXPR_INT_CONST:
		expression->datatype = env->type_int;
		break;
	case EXPR_REFERENCE:
		check_reference_expression(env, (reference_expression_t*) expression);
		break;
	case EXPR_BINARY:
		check_binary_expression(env, (binary_expression_t*) expression);
		break;
	case EXPR_UNARY:
		check_unary_expression(env, (unary_expression_t*) expression);
		break;
	case EXPR_SELECT:
		check_select_expression(env, (select_expression_t*) expression);
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
	method_t     *method             = env->current_method;
	type_t       *method_result_type = method->type->result_type;
	expression_t *return_value       = statement->return_value;

	env->last_statement_was_return = 1;

	if(return_value != NULL) {
		check_expression(env, return_value);
		if(method_result_type == void_type
				&& return_value->datatype != void_type) {
			fprintf(stderr, "Error: return with value in void method '%s'\n",
			        method->symbol->string);
			env->found_errors = 1;
			return;
		}

		/* do we need a cast ?*/
		if(return_value->datatype != method_result_type) {
			return_value
				= make_cast(env, return_value, method_result_type);

			statement->return_value = return_value;
		}
	} else {
		if(method_result_type != void_type) {
			fprintf(stderr, "Error: missing return value in method '%s'\n",
			        method->symbol->string);
			env->found_errors = 1;
			return;
		}
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
	env->next_valnum++;

	statement->refs         = 0;
	if(statement->type != NULL) {
		statement->type = normalize_type(env, statement->type);
		check_local_variable_type(env, statement, statement->type);
	}

	/* push the variable declaration on the environment stack */
	environment_entry_t *entry = environment_push(env, statement->symbol);
	entry->type                = ENTRY_LOCAL_VARIABLE;
	entry->e.variable          = statement;

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
			((binary_expression_t*) expression)->type == BINEXPR_ASSIGN)
		is_assign = 1;

	if(expression->datatype != void_type && !is_assign) {
		fprintf(stderr, "Warning: result of expression is unused\n");
		fprintf(stderr, "Note: Cast expression to void to avoid this warning\n");
	}
}

static
void check_statement(semantic_env_t *env, statement_t *statement)
{
	env->last_statement_was_return = 0;
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
		entry->e.method_parameter  = parameter;
		parameter->num             = n;
		parameter->type            = normalize_type(env, parameter->type);

		n++;
		parameter = parameter->next;
	}

	check_statement(env, method->statement);

	if(!env->last_statement_was_return) {
		type_t *result_type = method->type->result_type;
		if(result_type != void_type) {
			fprintf(stderr, "Error: Missing return statement at end of '%s'\n",
			        method->symbol->string);
			env->found_errors = 1;
			return;
		}
	}

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
	struct_t            *the_struct;
	int                  old_top        = environment_top(env);

	/* record namespace entries in environment */
	namespace_entry_t *entry = namespace->first_entry;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_VARIABLE:
			variable            = (variable_t*) entry;
			env_entry           = environment_push(env, variable->symbol);
			env_entry->type     = ENTRY_GLOBAL_VARIABLE;
			env_entry->e.global_variable = variable;
			break;
		case NAMESPACE_ENTRY_EXTERN_METHOD:
			extern_method       = (extern_method_t*) entry;
			env_entry           = environment_push(env, extern_method->symbol);
			env_entry->type     = ENTRY_EXTERN_METHOD;
			env_entry->e.extern_method = extern_method;
			break;
		case NAMESPACE_ENTRY_METHOD:
			method              = (method_t*) entry;
			env_entry           = environment_push(env, method->symbol);
			env_entry->type     = ENTRY_METHOD;
			env_entry->e.method = method;
			break;
		case NAMESPACE_ENTRY_STRUCT:
			the_struct             = (struct_t*) entry;
			env_entry              = environment_push(env, the_struct->symbol);
			env_entry->type        = ENTRY_TYPE;
			env_entry->e.stored_type = (type_t*) the_struct->type;
			break;
		default:
			panic("Unknown thing in namespace");
			break;
		}
		entry = entry->next;
	}

	/* normalize types */
	entry = namespace->first_entry;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_VARIABLE:
			variable            = (variable_t*) entry;
			env_entry           = variable->symbol->thing;
			assert(env_entry->e.global_variable == variable);
			variable->type      = normalize_type(env, variable->type);
			break;
		case NAMESPACE_ENTRY_EXTERN_METHOD:
			extern_method       = (extern_method_t*) entry;
			env_entry           = extern_method->symbol->thing;
			assert(env_entry->e.extern_method == extern_method);
			extern_method->type = (method_type_t*)
			    normalize_type(env, (type_t*) extern_method->type);
			break;
		case NAMESPACE_ENTRY_METHOD:
			method              = (method_t*) entry;
			env_entry           = method->symbol->thing;
			assert(env_entry->e.method == method);
			method->type 
				= (method_type_t*) normalize_type(env, (type_t*) method->type);
			break;
		case NAMESPACE_ENTRY_STRUCT:
			the_struct = (struct_t*) entry;
			env_entry  = the_struct->symbol->thing;
			assert(env_entry->e.stored_type == (type_t*) the_struct->type);
			env_entry->e.stored_type 
				= normalize_type(env, env_entry->e.stored_type);
			break;
		default:
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
