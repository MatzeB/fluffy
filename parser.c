#include <config.h>

#include "parser.h"

#include <assert.h>
#include <stdio.h>
#include <stdarg.h>

#include "symbol_table_t.h"
#include "known_symbols.h"
#include "lexer_t.h"
#include "symbol.h"
#include "ast_t.h"
#include "adt/obst.h"
#include "adt/util.h"

#define ABORT_ON_ERROR
#define LOOKAHEAD	1

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
	memmove(env->lookahead, env->lookahead + 1,
	        (LOOKAHEAD - 1) * sizeof(env->lookahead[0]));
	env->lookahead[LOOKAHEAD - 1] = lexer_next_token(&env->lexer);
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
	default:
		parse_error_expected(env, "couldn't parse type",
				T_byte, T_short, T_int, T_long, 0);
		return ATOMIC_TYPE_INVALID;
	}
}

static
type_t *parse_atomic_type(parser_env_t *env)
{
	atomic_type_t *type = obstack_alloc(&env->obst, sizeof(type[0]));
	memset(type, 0, sizeof(type[0]));
	type->type.type = TYPE_ATOMIC;

	switch(env->token.type) {
	case T_unsigned:
		next_token(env);
		type->atype = parse_unsigned_atomic_type(env);
		break;
	case T_signed:
		next_token(env);
		/* fallthrough */
	default:
		type->atype = parse_signed_atomic_type(env);
		break;
	}

	return (type_t*) type;
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
	symbol_t *symbol = env->token.symbol;
	next_token(env);
	return parse_type_ref_sym(env, symbol);
}

static
type_t *parse_type(parser_env_t *env)
{
	switch(env->token.type) {
	case T_unsigned:
	case T_signed:
	case T_int:
	case T_byte:
	case T_short:
	case T_float:
	case T_double:
		return parse_atomic_type(env);
	case T_IDENTIFIER:
		return parse_type_ref(env);
	}

	parse_error(env, "Couldn't parse type\n");
	type_t *type = obstack_alloc(&env->obst, sizeof(type[0]));
	type->type = TYPE_INVALID;
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
	cnst->value           = env->token.intvalue;

	next_token(env);

	return (expression_t*) cnst;
}

static
expression_t *parse_variable_reference(parser_env_t *env)
{
	variable_reference_expression_t *ref = 
		obstack_alloc(&env->obst, sizeof(ref[0]));
	memset(ref, 0, sizeof(ref[0]));

	ref->expression.type = EXPR_VARIABLE_REFERENCE;
	ref->symbol          = env->token.symbol;

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
		return parse_variable_reference(env);
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
expression_t *parse_postfix_expression(parser_env_t *env)
{
	return parse_primary_expression(env);
}

static
expression_t *parse_unary_expression(parser_env_t *env)
{
	return parse_postfix_expression(env);
}

static
expression_t *parse_multiplicative_expression(parser_env_t *env)
{
	expression_t *left = parse_unary_expression(env);

	while(1) {
		if(env->token.type == '*') {
			next_token(env);
			left = parse_unary_expression(env); /* TODO */
		} else if(env->token.type == '/') {
			next_token(env);
			left = parse_unary_expression(env); /* TODO */
		} else {
			break;
		}
	}

	return left;
}

static
expression_t *parse_additive_expression(parser_env_t *env)
{
	expression_t *left = parse_multiplicative_expression(env);

	while(1) {
		if(env->token.type == '+') {
			next_token(env);
			left = parse_multiplicative_expression(env); /* TODO */
		} else if(env->token.type == '-') {
			next_token(env);
			left = parse_multiplicative_expression(env); /* TODO */
		} else {
			break;
		}
	}

	return left;
}

static
expression_t *parse_equality_expression(parser_env_t *env)
{
	expression_t *left = parse_additive_expression(env);

	while(1) {
		if(env->token.type == T_EQUALEQUAL) {
			next_token(env);
			left = parse_additive_expression(env); /* TODO */
		} else if(env->token.type == T_EXCLAMATIONEQUAL) {
			next_token(env);
			left = parse_additive_expression(env); /* TODO */
		} else {
			break;
		}
	}

	return left;
}

static
expression_t *parse_assign_expression(parser_env_t *env)
{
	expression_t *left = parse_equality_expression(env);

	while(env->token.type == '=') {
		next_token(env);

		assign_expression_t *assign 
			= obstack_alloc(&env->obst, sizeof(assign[0]));
		memset(assign, 0, sizeof(assign[0]));
		
		assign->expression.type = EXPR_ASSIGN;
		assign->left            = left;
		assign->right           = parse_equality_expression(env);
		
		left = (expression_t*) assign;
	}

	return left;
}

static
expression_t *parse_expression(parser_env_t *env)
{
	return parse_assign_expression(env);
}

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

	return (statement_t*) return_statement;
}

static
statement_t *parse_initial_assignment(parser_env_t *env, symbol_t *symbol)
{
	variable_reference_expression_t *ref 
		= obstack_alloc(&env->obst, sizeof(ref[0]));
	ref->expression.type = EXPR_VARIABLE_REFERENCE;
	ref->symbol          = symbol;

	assign_expression_t *assign 
		= obstack_alloc(&env->obst, sizeof(assign[0]));
	memset(assign, 0, sizeof(assign[0]));

	assign->expression.type = EXPR_ASSIGN;
	assign->left  = (expression_t*) ref;
	assign->right = parse_expression(env);

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

	type_t                           *type = parse_type(env);
	
	while(1) {
		decl = obstack_alloc(&env->obst, sizeof(decl[0]));
		memset(decl, 0, sizeof(decl[0]));
		decl->statement.type = STATEMENT_VARIABLE_DECLARATION;
		decl->type           = type;
		decl->symbol         = env->token.symbol;

		/* append multiple variable declarations */
		if(last_statement != NULL) {
			last_statement->next = (statement_t*) decl;
		} else {
			first_statement = (statement_t*) decl;
		}
		last_statement = (statement_t*) decl;
		next_token(env);

		/* do we have an assignment expression? */
		if(env->token.type == '=') {
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

	return first_statement;
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
	case '{':
		next_token(env);
		statement = parse_block(env);
		expect(env, '}');
		return statement;
	case T_unsigned:
	case T_signed:
	case T_byte:
	case T_short:
	case T_int:
	case T_long:
		statement = parse_variable_declaration(env);
		break;
	case T_IDENTIFIER:
		if(la(env, 1)->type == T_IDENTIFIER) {
			/* must be a variable declaration */
			statement = parse_variable_declaration(env);
			break;
		}
		
		/* must be some expression starting with a variable reference */
		expression_statement_t *expression_statement
			= obstack_alloc(&env->obst, sizeof(expression_statement[0]));
		memset(expression_statement, 0, sizeof(expression_statement[0]));

		expression_statement->statement.type = STATEMENT_EXPRESSION;
		expression_statement->expression     = parse_expression(env);
		statement = (statement_t*) expression_statement;
		break;
	default:
		parse_error(env, "Expected statement");
		eat_until_semi(env);
		statement = obstack_alloc(&env->obst, sizeof(statement[0]));
		memset(statement, 0, sizeof(statement[0]));
		statement->type = STATEMENT_INVALID;
		return statement;
	}

	expect(env, ';');
	return statement;
}

static
statement_t *parse_block(parser_env_t *env)
{
	statement_t       *last = NULL;
	block_statement_t *block = obstack_alloc(&env->obst, sizeof(block[0]));
	memset(block, 0, sizeof(block[0]));

	block->statement.type = STATEMENT_BLOCK;

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

	return (statement_t*) block;
}

static
namespace_entry_t *parse_function_or_var(parser_env_t *env)
{
	type_t   *type   = parse_type(env);
	symbol_t *symbol;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, NULL, T_IDENTIFIER, 0);
		eat_until_semi(env);
		return NULL;
	}
	symbol = env->token.symbol;
	next_token(env);

	/* is it a function? */
	if(env->token.type == '(') {
		function_t *function = obstack_alloc(&env->obst, sizeof(function[0]));
		memset(function, 0, sizeof(function[0]));

		function->namespace_entry.type = NAMESPACE_ENTRY_FUNCTION;
		function->symbol               = symbol;
		function->return_type          = type;

		/* TODO: free memory in case of error... */

		next_token(env);
		expect(env, ')');
		expect(env, '{');
		function->statement = parse_block(env);
		expect(env, '}');

		return (namespace_entry_t*) function;
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
namespace_t *parse_namespace(parser_env_t *env)
{
	namespace_t *namespace = obstack_alloc(&env->obst, sizeof(namespace[0]));
	memset(namespace, 0, sizeof(namespace[0]));

	while(1) {
		switch(env->token.type) {
		case T_unsigned:
		case T_signed:
		case T_int:
		case T_byte:
		case T_short:
		case T_float:
		case T_double: {
			namespace_entry_t *entry = parse_function_or_var(env);

			if(entry != NULL) {
				entry->next = namespace->first_entry;
				namespace->first_entry = entry;
			}
			continue;
		}

		case ';':
			break;
		case T_EOF:
			return namespace;
		default:
			parse_error(env, "Couldn't parse compilation unit entry");
			break;
		}
		next_token(env);
	}
}

namespace_t *parse(FILE *in, const char *input_name)
{
	int i;
	parser_env_t env;
	memset(&env, 0, sizeof(env));

	obstack_init(&env.obst);

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
	obstack_free(&env.obst, NULL); */

	if(env.error) {
		fprintf(stderr, "Errors happened...\n");
		return NULL;
	}

	return unit;
}

