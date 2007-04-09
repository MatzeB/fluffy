#include <config.h>

#include "parser.h"

#include <assert.h>
#include <stdio.h>
#include <stdarg.h>

#include "symbol_table_t.h"
#include "lexer_t.h"
#include "symbol.h"
#include "type_hash.h"
#include "ast_t.h"
#include "adt/obst.h"
#include "adt/util.h"
#include "adt/error.h"

#define ABORT_ON_ERROR
#define LOOKAHEAD	1
#define PRINT_TOKENS

typedef struct {
	struct obstack  obst;
	token_t         token;
	token_t         lookahead[LOOKAHEAD];
	lexer_t         lexer;
	symbol_table_t  symbol_table;
	int             error;
} parser_env_t;

static inline
void next_token(parser_env_t *env)
{
	env->token = env->lookahead[0];
#if LOOKAHEAD > 1
	memmove(env->lookahead, env->lookahead + 1,
	        (LOOKAHEAD - 1) * sizeof(env->lookahead[0]));
#endif
	lexer_next_token(&env->lexer, & env->lookahead[LOOKAHEAD - 1]);

#ifdef PRINT_TOKENS
	print_token(stderr, & env->lookahead[LOOKAHEAD - 1]);
	fprintf(stderr, "\n");
#endif
}

/** look ahead some symbols */
static inline
const token_t* la(parser_env_t *env, int i)
{
	assert(i >= 1);
	assert(i <= LOOKAHEAD);
	return & env->lookahead[i - 1];
}

typedef enum {
	ERR_WARNING,
	ERR_ERROR
} error_type_t;

static inline
void parser_found_error(parser_env_t *env)
{
	env->error = 1;
#ifdef ABORT_ON_ERROR
	abort();
#endif
}

static
void print_error_prefix(parser_env_t *env, error_type_t error_type)
{
	fputs(env->token.sourcefile, stderr);
	fputc(':', stderr);
	fprintf(stderr, "%d", env->token.linenr);
	if(error_type == ERR_WARNING) {
		fputs(": warning: ", stderr);
	} else {
		fputs(": error: ", stderr);
	}
}

static
void parse_error(parser_env_t *env, const char *message)
{
	print_error_prefix(env, ERR_ERROR);
	fprintf(stderr, "Parse error: %s\n", message);
	parser_found_error(env);
}

static
void parse_error_expected(parser_env_t *env, const char *message, ...)
{
	va_list args;
	int first = 1;

	if(message != NULL) {
		print_error_prefix(env, ERR_ERROR);
		fprintf(stderr, "%s\n", message);
	}
	print_error_prefix(env, ERR_ERROR);
	fputs("Parse error: got ", stderr);
	print_token(stderr, &env->token);
	fputs(", expected ", stderr);

	va_start(args, message);
	token_type_t token_type = va_arg(args, token_type_t);
	while(token_type != 0) {
		if(first == 1) {
			first = 0;
		} else {
			fprintf(stderr, ", ");
		}
		print_token_type(stderr, token_type);
		token_type = va_arg(args, token_type_t);
	}
	va_end(args);
	fprintf(stderr, "\n");
	parser_found_error(env);
}


static type_t void_type_    = { TYPE_VOID, NULL };
static type_t invalid_type_ = { TYPE_INVALID, NULL };
type_t *void_type    = &void_type_;
type_t *invalid_type = &invalid_type_;

static
atomic_type_type_t parse_unsigned_atomic_type(parser_env_t *env)
{
	switch(env->token.type) {
	case T_byte:
		next_token(env);
		return ATOMIC_TYPE_UBYTE;
	case T_short:
		next_token(env);
		return ATOMIC_TYPE_USHORT;
	case T_long:
		next_token(env);
		if(env->token.type == T_long) {
			next_token(env);
			return ATOMIC_TYPE_ULONGLONG;
		}
		return ATOMIC_TYPE_ULONG;
	case T_int:
		next_token(env);
		return ATOMIC_TYPE_UINT;
	default:
		parse_error_expected(env, "couldn't parse type",
				T_byte, T_short, T_int, T_long, 0);
		return ATOMIC_TYPE_INVALID;
	}
}

static
atomic_type_type_t parse_signed_atomic_type(parser_env_t *env)
{
	switch(env->token.type) {
	case T_byte:
		next_token(env);
		return ATOMIC_TYPE_BYTE;
	case T_short:
		next_token(env);
		return ATOMIC_TYPE_SHORT;
	case T_long:
		next_token(env);
		if(env->token.type == T_long) {
			next_token(env);
			return ATOMIC_TYPE_LONGLONG;
		}
		return ATOMIC_TYPE_LONG;
	case T_int:
		next_token(env);
		return ATOMIC_TYPE_INT;
	case T_float:
		next_token(env);
		return ATOMIC_TYPE_FLOAT;
	case T_double:
		next_token(env);
		return ATOMIC_TYPE_DOUBLE;
	default:
		parse_error_expected(env, "couldn't parse type",
				T_byte, T_short, T_int, T_long, T_float, T_double, 0);
		return ATOMIC_TYPE_INVALID;
	}
}

static
type_t *parse_atomic_type(parser_env_t *env)
{
	atomic_type_type_t atype;

	switch(env->token.type) {
	case T_unsigned:
		next_token(env);
		atype = parse_unsigned_atomic_type(env);
		break;
	case T_signed:
		next_token(env);
		/* fallthrough */
	default:
		atype = parse_signed_atomic_type(env);
		break;
	}

	atomic_type_t *type = obstack_alloc(&env->obst, sizeof(type[0]));
	memset(type, 0, sizeof(type[0]));
	type->type.type = TYPE_ATOMIC;
	type->atype = atype;

	type_t *result = typehash_insert((type_t*) type);
	if(result != (type_t*) type) {
		obstack_free(&env->obst, type);
	}

	return result;
}

static
type_t *parse_type_ref_sym(parser_env_t *env, symbol_t *symbol)
{
	ref_type_t *type_ref = obstack_alloc(&env->obst, sizeof(type_ref[0]));
	memset(type_ref, 0, sizeof(type_ref[0]));

	type_ref->type.type = TYPE_REF;
	type_ref->symbol    = symbol;
	return (type_t*) type_ref;
}

static
type_t *parse_type_ref(parser_env_t *env)
{
	symbol_t *symbol = env->token.v.symbol;
	next_token(env);
	return parse_type_ref_sym(env, symbol);
}

static
type_t *parse_type(parser_env_t *env)
{
	type_t *type;

	switch(env->token.type) {
	case T_unsigned:
	case T_signed:
	case T_int:
	case T_byte:
	case T_short:
	case T_float:
	case T_double:
		type = parse_atomic_type(env);
		break;
	case T_IDENTIFIER:
		type = parse_type_ref(env);
		break;
	case T_void:
		type = void_type;
		next_token(env);
		break;
	default:
		parse_error(env, "Invalid type");
		type = invalid_type;
		break;
	}

	while(env->token.type == '*') {
		next_token(env);

		pointer_type_t *pointer_type 
			= obstack_alloc(&env->obst, sizeof(pointer_type[0]));
		memset(pointer_type, 0, sizeof(pointer_type[0]));

		pointer_type->type.type = TYPE_POINTER;
		pointer_type->points_to = type;

		type = (type_t*) pointer_type;
	}

	return type;
}

static
void eat_until_semi(parser_env_t *env)
{
	while(env->token.type != ';') {
		next_token(env);
		if(env->token.type == T_EOF)
			return;
	}
	next_token(env);
}

#define expect(env,expected) \
	if(UNLIKELY(env->token.type != (expected))) { \
		parse_error_expected(env, NULL, (expected), 0); \
		eat_until_semi(env); \
		return NULL; \
	} \
	next_token(env);

static
expression_t *parse_int_const(parser_env_t *env)
{
	int_const_t *cnst = obstack_alloc(&env->obst, sizeof(cnst[0]));
	memset(cnst, 0, sizeof(cnst));

	cnst->expression.type = EXPR_INT_CONST;
	cnst->value           = env->token.v.intvalue;

	next_token(env);

	return (expression_t*) cnst;
}

static
expression_t *parse_reference(parser_env_t *env)
{
	reference_expression_t *ref = obstack_alloc(&env->obst, sizeof(ref[0]));
	memset(ref, 0, sizeof(ref[0]));

	ref->expression.type = EXPR_REFERENCE;
	ref->symbol          = env->token.v.symbol;

	next_token(env);

	return (expression_t*) ref;
}

static
expression_t *parse_expression(parser_env_t *env);

static
expression_t *parse_primary_expression(parser_env_t *env)
{
	expression_t *expression;

	switch(env->token.type) {
	case T_INTEGER:
		return parse_int_const(env);
	case T_IDENTIFIER:
		return parse_reference(env);
	case '(':
		next_token(env);
		expression = parse_expression(env);
		expect(env, ')');
		break;
	default:
		parse_error_expected(env, "Expected expression", 0);
		expression = obstack_alloc(&env->obst, sizeof(expression[0]));
		memset(expression, 0, sizeof(expression[0]));
		expression->type = EXPR_INVALID;
		break;
	}

	return expression;
}

static
expression_t *parse_call_expression(parser_env_t *env, expression_t *expression)
{
	call_expression_t *call = obstack_alloc(&env->obst, sizeof(call[0]));
	memset(call, 0, sizeof(call[0]));

	call->expression.type = EXPR_CALL;
	call->method          = expression;

	/* parse arguments */
	assert(env->token.type == '(');
	next_token(env);

	if(env->token.type != ')') {
		call_argument_t *last_argument = NULL;

		while(1) {
			call_argument_t *argument
				= obstack_alloc(&env->obst, sizeof(argument[0]));
			memset(argument, 0, sizeof(argument[0]));

			argument->expression = parse_expression(env);
			if(last_argument == NULL) {
				call->arguments = argument;
			} else {
				last_argument->next = argument;
			}
			last_argument = argument;

			if(env->token.type != ',')
				break;
			next_token(env);
		}
	}
	expect(env, ')');

	return (expression_t*) call;
}

static
expression_t *parse_select_expression(parser_env_t *env, expression_t *compound)
{
	assert(env->token.type == '.');
	next_token(env);

	select_expression_t *select = obstack_alloc(&env->obst, sizeof(select[0]));
	memset(select, 0, sizeof(select[0]));

	select->expression.type = EXPR_SELECT;
	select->compound        = compound;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing compound select",
		                     T_IDENTIFIER, 0);
		return NULL;
	}
	select->symbol          = env->token.v.symbol;
	next_token(env);

	return (expression_t*) select;
}

static
expression_t *parse_postfix_expression(parser_env_t *env)
{
	expression_t *expression = parse_primary_expression(env);

	if(env->token.type == '(') {
		return parse_call_expression(env, expression);
	} else if(env->token.type == '.') {
		return parse_select_expression(env, expression);
	}

	return expression;
}

/**
 * Returns an unary expression type if c is a prefix operator
 */
static
unary_expression_type_t get_unexpr_prefix_type(int c)
{
	switch(c) {
	case '-':
		return UNEXPR_NEGATE;
	case '!':
		return UNEXPR_NOT;
	case '*':
		return UNEXPR_DEREFERENCE;
	case '&':
		return UNEXPR_TAKE_ADDRESS;
	default:
		return 0;
	}
}

static
expression_t *parse_prefix_expression(parser_env_t *env)
{
	/* TODO: integrate this into parse_expression_prec, so that prefix
	 * expressions can have configurable precedence too.
	 */
	unary_expression_type_t type = get_unexpr_prefix_type(env->token.type);
	if(type != 0) {
		next_token(env);
	}

	expression_t *value = parse_postfix_expression(env);

	if(type != 0) {
		unary_expression_t *unary_expression 
			= obstack_alloc(&env->obst, sizeof(unary_expression[0]));
		memset(unary_expression, 0, sizeof(unary_expression[0]));
		unary_expression->expression.type = EXPR_UNARY;
		unary_expression->type            = type;
		unary_expression->value           = value;

		value = (expression_t*) unary_expression;	
	}

	return value;
}

static
binary_expression_type_t get_binexpr_type(int c)
{
	switch(c) {
	case '*':
		return BINEXPR_MUL;
	case '/':
		return BINEXPR_DIV;
	case '%':
		return BINEXPR_MOD;
	case '+':
		return BINEXPR_ADD;
	case '-':
		return BINEXPR_SUB;
	case '<':
		return BINEXPR_LESS;
	case '>':
		return BINEXPR_GREATER;
	case '=':
		return BINEXPR_EQUAL;
	case T_ASSIGN:
		return BINEXPR_ASSIGN;
	case T_SLASHEQUAL:
		return BINEXPR_NOTEQUAL;
	case T_LESSEQUAL:
		return BINEXPR_LESSEQUAL;
	case T_GREATEREQUAL:
		return BINEXPR_GREATEREQUAL;
	case '&':
		return BINEXPR_AND;
	case '|':
		return BINEXPR_OR;
	case '^':
		return BINEXPR_XOR;
	case T_LESSLESS:
		return BINEXPR_SHIFTLEFT;
	case T_GREATERGREATER:
		return BINEXPR_SHIFTRIGHT;
	default:
		panic("Can't convert unknown operator to binexpr_type");
	}
}

static
unsigned get_level(int c)
{
	switch(c) {
	case T_ASSIGN:
		return 2;

	case '|':
		return 10;

	case '^':
		return 11;

	case '&':
		return 12;

	case '=':
	case T_SLASHEQUAL:
		return 13;

	case '<':
	case '>':
	case T_LESSEQUAL:
	case T_GREATEREQUAL:
		return 14;

	case '+':
	case '-':
		return 15;

	case '*':
	case '/':
	case '%':
	case T_LESSLESS:
	case T_GREATERGREATER:
		return 16;

	default:
		return 0;
	}
}

static
expression_t *parse_expression_prec(parser_env_t *env, unsigned level)
{
	expression_t *left = parse_prefix_expression(env);

	while(1) {
		int      op       = env->token.type;
		unsigned op_level = get_level(op);
		if(op_level < level)
			return left;

		next_token(env);
		expression_t *right = parse_expression_prec(env, op_level + 1);

		binary_expression_t *binexpr
			= obstack_alloc(&env->obst, sizeof(binexpr[0]));
		memset(binexpr, 0, sizeof(binexpr[0]));
		binexpr->expression.type = EXPR_BINARY;
		binexpr->type            = get_binexpr_type(op);
		binexpr->left            = left;
		binexpr->right           = right;

		left = (expression_t*) binexpr;
	}

	return left;
}

static
expression_t *parse_expression(parser_env_t *env)
{
	return parse_expression_prec(env, 1);
}

static
statement_t *parse_statement(parser_env_t *env);

static
statement_t *parse_return_statement(parser_env_t *env)
{
	return_statement_t *return_statement =
		obstack_alloc(&env->obst, sizeof(return_statement[0]));
	memset(return_statement, 0, sizeof(return_statement[0]));

	return_statement->statement.type = STATEMENT_RETURN;
	next_token(env);

	if(env->token.type != ';') {
		return_statement->return_value = parse_expression(env);
	}

	expect(env, ';');

	return (statement_t*) return_statement;
}

static
statement_t *parse_if_statement(parser_env_t *env)
{
	expression_t *condition;
	statement_t  *true_statement;
	statement_t  *false_statement = NULL;

	assert(env->token.type == T_if);
	next_token(env);

	expect(env, '(');
	condition = parse_expression(env);
	expect(env, ')');

	true_statement = parse_statement(env);
	if(env->token.type == T_else) {
		next_token(env);
		false_statement = parse_statement(env);
	}

	if_statement_t *if_statement
		= obstack_alloc(&env->obst, sizeof(if_statement[0]));
	memset(if_statement, 0, sizeof(if_statement[0]));

	if_statement->statement.type  = STATEMENT_IF;
	if_statement->condition       = condition;
	if_statement->true_statement  = true_statement;
	if_statement->false_statement = false_statement;

	return (statement_t*) if_statement;
}

static
statement_t *parse_initial_assignment(parser_env_t *env, symbol_t *symbol)
{
	reference_expression_t *ref = obstack_alloc(&env->obst, sizeof(ref[0]));
	ref->expression.type = EXPR_REFERENCE;
	ref->symbol          = symbol;

	binary_expression_t *assign = obstack_alloc(&env->obst, sizeof(assign[0]));
	memset(assign, 0, sizeof(assign[0]));

	assign->expression.type = EXPR_BINARY;
	assign->type            = BINEXPR_ASSIGN;
	assign->left            = (expression_t*) ref;
	assign->right           = parse_expression(env);

	expression_statement_t *expr_statement
		= obstack_alloc(&env->obst, sizeof(expr_statement[0]));
	memset(expr_statement, 0, sizeof(expr_statement[0]));

	expr_statement->statement.type = STATEMENT_EXPRESSION;
	expr_statement->expression     = (expression_t*) assign;

	return (statement_t*) expr_statement;
}

static
statement_t *parse_variable_declaration(parser_env_t *env)
{
	statement_t                      *first_statement = NULL;
	statement_t                      *last_statement  = NULL;
	variable_declaration_statement_t *decl;
	type_t                           *type            = NULL;

	assert(env->token.type == T_def);
	next_token(env);

	if(env->token.type == '<') {
		next_token(env);
		type = parse_type(env);
		expect(env, '>');
	}

	while(1) {
		decl = obstack_alloc(&env->obst, sizeof(decl[0]));
		memset(decl, 0, sizeof(decl[0]));
		decl->statement.type = STATEMENT_VARIABLE_DECLARATION;
		decl->type           = type;
		decl->symbol         = env->token.v.symbol;

		/* append multiple variable declarations */
		if(last_statement != NULL) {
			last_statement->next = (statement_t*) decl;
		} else {
			first_statement = (statement_t*) decl;
		}
		last_statement = (statement_t*) decl;
		next_token(env);

		/* do we have an assignment expression? */
		if(env->token.type == T_ASSIGN) {
			next_token(env);
			statement_t *assign = parse_initial_assignment(env, decl->symbol);

			last_statement->next = assign;
			last_statement = assign;
		}

		/* check if we have more declared symbols separated by ',' */
		if(env->token.type != ',')
			break;
		next_token(env);

		/* there must be another identifier after the comma */
		if(env->token.type != T_IDENTIFIER) {
			parse_error_expected(env, "Problem parsing variable declaration",
			                     T_IDENTIFIER, 0);
			eat_until_semi(env);
			return NULL;
		}
	}

	expect(env, ';');
	return first_statement;
}

static
statement_t *parse_expression_statement(parser_env_t *env)
{
	expression_statement_t *expression_statement
		= obstack_alloc(&env->obst, sizeof(expression_statement[0]));
	memset(expression_statement, 0, sizeof(expression_statement[0]));

	expression_statement->statement.type = STATEMENT_EXPRESSION;
	expression_statement->expression     = parse_expression(env);
	expect(env, ';');

	return (statement_t*) expression_statement;
}

static
statement_t *parse_block(parser_env_t *env);

static
statement_t *parse_statement(parser_env_t *env)
{
	statement_t *statement;

	switch(env->token.type) {
	case T_return:
		statement = parse_return_statement(env);
		break;

	case T_if:
		statement = parse_if_statement(env);
		break;

	case '{':
		statement = parse_block(env);
		break;

	case T_def:
		statement = parse_variable_declaration(env);
		break;

	default:
		statement = parse_expression_statement(env);
		break;
	}

	return statement;
}

static
statement_t *parse_block(parser_env_t *env)
{
	statement_t       *last = NULL;
	block_statement_t *block = obstack_alloc(&env->obst, sizeof(block[0]));
	memset(block, 0, sizeof(block[0]));
	block->statement.type = STATEMENT_BLOCK;

	assert(env->token.type == '{');
	next_token(env);

	while(env->token.type != '}') {
		/* eat the empty statement */
		if(env->token.type == ';') {
			next_token(env);
			continue;
		}

		/* parse statement */
		statement_t *statement = parse_statement(env);
		if(last != NULL) {
			last->next = statement;
		} else {
			block->first_statement = statement;
		}
		last = statement;
		/* the parse rule might have produced multiple statements */
		while(last->next != NULL)
			last = last->next;
	}

	expect(env, '}');

	return (statement_t*) block;
}

static
void parse_parameter_declaration(parser_env_t *env,
                                 method_parameter_type_t **parameter_types,
                                 method_parameter_t **parameters)
{
	if(env->token.type == ')')
		return;

	method_parameter_type_t *last_type = NULL;
	method_parameter_t      *last_param = NULL;
	if(parameter_types != NULL)
		*parameter_types = NULL;
	if(parameters != NULL)
		*parameters = NULL;

	while(1) {
		type_t *type;

		switch(env->token.type) {
		case T_unsigned:
		case T_signed:
		case T_int:
		case T_byte:
		case T_short:
		case T_float:
		case T_double:
		case T_void:
		case T_IDENTIFIER:
			type = parse_type(env);

			if(parameter_types != NULL) {
				method_parameter_type_t *param_type
					= obstack_alloc(&env->obst, sizeof(param_type[0]));
				memset(param_type, 0, sizeof(param_type[0]));
				param_type->type = type;

				if(last_type != NULL) {
					last_type->next = param_type;
				} else {
					*parameter_types = param_type;
				}
				last_type = param_type;
			}

			symbol_t *symbol = NULL;
			if(env->token.type == T_IDENTIFIER) {
				symbol = env->token.v.symbol;
				next_token(env);
			}

			if(parameters != NULL) {
				method_parameter_t *method_param
					= obstack_alloc(&env->obst, sizeof(method_param[0]));
				memset(method_param, 0, sizeof(method_param[0]));
				method_param->symbol = symbol;
				method_param->type   = type;

				if(last_param != NULL) {
					last_param->next = method_param;
				} else {
					*parameters = method_param;
				}
				last_param = method_param;
			}
			break;

		case T_DOTDOTDOT:
			panic("Variadic functions not implemented yet");
			next_token(env);
			break;
		default:
			parse_error(env, "Problem while parsing parameter: Expected type");
		}

		if(env->token.type != ',')
			break;
		next_token(env);
	}
}

static
namespace_entry_t *parse_method_or_var(parser_env_t *env)
{
	type_t   *type   = parse_type(env);
	symbol_t *symbol;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, NULL, T_IDENTIFIER, 0);
		eat_until_semi(env);
		return NULL;
	}
	symbol = env->token.v.symbol;
	next_token(env);

	/* is it a method? */
	if(env->token.type == '(') {
		next_token(env);

		method_t *method = obstack_alloc(&env->obst, sizeof(method[0]));
		memset(method, 0, sizeof(method[0]));

		method->namespace_entry.type = NAMESPACE_ENTRY_METHOD;
		method->symbol               = symbol;

		method_type_t *method_type
			= obstack_alloc(&env->obst, sizeof(method_type[0]));
		memset(method_type, 0, sizeof(method_type[0]));

		method_type->type.type   = TYPE_METHOD;
		method_type->result_type = type;

		/* parse parameters */
		parse_parameter_declaration(env, &method_type->parameter_types,
		                            &method->parameters);

		method->type = method_type;

		expect(env, ')');
		if(env->token.type != '{') {
			parse_error_expected(env, "Problem while parsing method", '{', 0);
			eat_until_semi(env);
			return NULL;
		}
		method->statement = parse_block(env);

		return (namespace_entry_t*) method;
	} else {
		/* must be a variable */
		expect(env, ';');

		variable_t *variable = obstack_alloc(&env->obst, sizeof(variable[0]));
		memset(variable, 0, sizeof(variable[0]));

		variable->namespace_entry.type = NAMESPACE_ENTRY_VARIABLE;
		variable->symbol               = symbol;
		variable->type                 = type;

		return (namespace_entry_t*) variable;
	}
}

static
namespace_entry_t *parse_extern_method(parser_env_t *env)
{
	const char              *abi_style   = NULL;
	type_t                  *result_type;

	extern_method_t *extern_method
		= obstack_alloc(&env->obst, sizeof(extern_method[0]));
	memset(extern_method, 0, sizeof(extern_method[0]));

	extern_method->namespace_entry.type = NAMESPACE_ENTRY_EXTERN_METHOD;

	/* skip the "extern" */
	assert(env->token.type == T_extern);
	next_token(env);

	if(env->token.type == T_STRING_LITERAL) {
		abi_style = env->token.v.string;
		next_token(env);
	}

	result_type = parse_type(env);

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing extern declaration",
		                     T_IDENTIFIER, 0);
		eat_until_semi(env);
		return NULL;
	}
	extern_method->symbol = env->token.v.symbol;
	next_token(env);

	method_type_t *method_type
		= obstack_alloc(&env->obst, sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));

	method_type->type.type       = TYPE_METHOD;
	method_type->abi_style       = abi_style;
	method_type->result_type     = result_type;

	expect(env, '(');
	parse_parameter_declaration(env, &method_type->parameter_types, NULL);
	expect(env, ')');
	expect(env, ';');

	extern_method->type = method_type;

	return (namespace_entry_t*) extern_method;
}

static
namespace_entry_t *parse_struct(parser_env_t *env)
{
	struct_t *struct_ = obstack_alloc(&env->obst, sizeof(struct_[0]));
	memset(struct_, 0, sizeof(struct_[0]));
	struct_->namespace_entry.type = NAMESPACE_ENTRY_STRUCT;

	assert(env->token.type == T_struct);
	next_token(env);

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing struct",
		                     T_IDENTIFIER, 0);
		eat_until_semi(env);
		return NULL;
	}
	struct_->symbol = env->token.v.symbol;
	next_token(env);

	expect(env, '{');

	struct_type_t *struct_type 
		= obstack_alloc(&env->obst, sizeof(struct_type[0]));
	memset(struct_type, 0, sizeof(struct_type[0]));

	struct_type->type.type     = TYPE_STRUCT;
	struct_type->symbol        = struct_->symbol;
	struct_entry_t *last_entry = NULL;
	while(env->token.type != '}') {
		struct_entry_t *entry = obstack_alloc(&env->obst, sizeof(entry[0]));
		memset(entry, 0, sizeof(entry[0]));

		entry->type = parse_type(env);
		if(env->token.type != T_IDENTIFIER) {
			parse_error_expected(env, "Problem while parsing struct entry",
			                     T_IDENTIFIER, 0);
			return NULL;
		}
		entry->symbol = env->token.v.symbol;
		next_token(env);

		if(last_entry == NULL) {
			struct_type->entries = entry;
		} else {
			last_entry->next     = entry;
		}
		last_entry = entry;

		expect(env, ';');
	}

	expect(env, '}');
	struct_->type = struct_type;

	return (namespace_entry_t*) struct_;
}

static
namespace_t *parse_namespace(parser_env_t *env)
{
	namespace_t *namespace = obstack_alloc(&env->obst, sizeof(namespace[0]));
	memset(namespace, 0, sizeof(namespace[0]));

	while(1) {
		namespace_entry_t *entry = NULL;

		switch(env->token.type) {
		case T_unsigned:
		case T_signed:
		case T_int:
		case T_byte:
		case T_short:
		case T_float:
		case T_double:
		case T_void:
		case T_IDENTIFIER:
			entry = parse_method_or_var(env);
			break;

		case T_extern:
			entry = parse_extern_method(env);
			break;

		case T_struct:
			entry = parse_struct(env);
			break;

		case ';':
			next_token(env);
			continue;

		case T_EOF:
			return namespace;

		default:
			parse_error(env, "Couldn't parse compilation unit entry");
			next_token(env);
			break;
		}

		if(entry != NULL) {
			entry->next = namespace->first_entry;
			namespace->first_entry = entry;
		}
	}
}

namespace_t *parse(FILE *in, const char *input_name)
{
	int i;
	parser_env_t env;
	memset(&env, 0, sizeof(env));

	obstack_init(&env.obst);
	typehash_init();

	symbol_table_init(&env.symbol_table);
	put_known_symbols_into_symbol_table(&env.symbol_table);

	lexer_init(&env.lexer, &env.symbol_table, in, input_name);
	for(i = 0; i < LOOKAHEAD + 1; ++i) {
		next_token(&env);
	}

	namespace_t *unit = parse_namespace(&env);

	lexer_destroy(&env.lexer);
	/* FIXME: make these global somehow
	symbol_table_destroy(&env.symbol_table);
	obstack_free(&env.obst, NULL);
	typehash_destroy();
	*/

	if(env.error) {
		fprintf(stderr, "Errors happened...\n");
		return NULL;
	}

	return unit;
}
