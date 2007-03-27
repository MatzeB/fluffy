#include <config.h>

#include "parser.h"

#include <stdio.h>
#include <stdarg.h>

#include "symbol_table_t.h"
#include "known_symbols.h"
#include "lexer_t.h"
#include "symbol.h"
#include "ast_t.h"
#include "adt/obst.h"
#include "adt/util.h"

typedef struct {
	struct obstack obst;
	token_t token;
	lexer_t lexer;
	symbol_table_t symbol_table;
	int error;
} parser_env_t;

static inline
void next_token(parser_env_t *env)
{
	env->token = lexer_next_token(&env->lexer);
}

typedef enum {
	ERR_WARNING,
	ERR_ERROR
} error_type_t;

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
	env->error = 1;
}

static
void parse_error(parser_env_t *env, const char *message)
{
	print_error_prefix(env, ERR_ERROR);
	fprintf(stderr, "Parse error: %s\n", message);
	env->error = 1;
}

static
void parse_error_expected(parser_env_t *env, const char *message, ...)
{
	va_list args;
	int token;
	int first = 0;

	if(message != NULL) {
		print_error_prefix(env, ERR_ERROR);
		fprintf(stderr, "%s\n", message);
	}
	print_error_prefix(env, ERR_ERROR);
	fprintf(stderr, "Parse error, got '%d' expected '", env->token.type);
	va_start(args, message);
	token = va_arg(args, int);
	while(token != 0) {
		if(first == 1) {
			first = 0;
		} else {
			fprintf(stderr, ", ");
		}
		fprintf(stderr, "%d", token);
		token = va_arg(args, int);
	}
	va_end(args);
	fprintf(stderr, "'\n");
}

static
type_t *parse_atomic_type(parser_env_t *env)
{
	int had_sign_modifier = 0;

	atomic_type_t *type = obstack_alloc(&env->obst, sizeof(type[0]));
	memset(type, 0, sizeof(type[0]));
	type->type.type = TYPE_ATOMIC;
	type_t *res = (type_t*) type;

	while(1) {
		switch(env->token.type) {
		case T_unsigned:
			if(had_sign_modifier) {
				parse_error(env, "Skipping double sign modifier 'unsigned'");
			} else {
				type->has_sign = 0;
				had_sign_modifier = 1;
			}
			break;
		case T_signed:
			if(had_sign_modifier) {
				parse_error(env, "Skipping double sign modifier 'signed'");
			} else {
				type->has_sign = 1;
				had_sign_modifier = 1;
			}
			break;
		case T_long:
			next_token(env);
			if(env->token.type == T_long) {
				type->atype = ATOMIC_TYPE_INT;
				type->bits  = 64;
				next_token(env);
			} else {
				type->atype = ATOMIC_TYPE_INT;
				type->bits  = 32;
			}
			return res;
		case T_int:
			type->atype = ATOMIC_TYPE_INT;
			type->bits  = 32;
			next_token(env);
			return res;
		case T_short:
			type->atype = ATOMIC_TYPE_INT;
			type->bits  = 16;
			next_token(env);
			return res;
		case T_char:
			type->atype = ATOMIC_TYPE_INT;
			type->bits  = 8;
			next_token(env);
			return res;
		default:
			parse_error_expected(env, "couldn't parse type",
			                     T_unsigned, T_signed, T_long, T_int, T_short,
								 T_char, 0);
			/* return the invalid type */
			type->type.type = TYPE_INVALID;
			return res;
		}
		next_token(env);
	}
}

static
type_t *parse_type(parser_env_t *env)
{
	switch(env->token.type) {
	case T_unsigned:
	case T_signed:
	case T_int:
	case T_char:
	case T_short:
	case T_float:
	case T_double:
		return parse_atomic_type(env);
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
expression_t *parse_int_atom(parser_env_t *env)
{
	int_atom_t *atom = obstack_alloc(&env->obst, sizeof(atom[0]));
	atom->expression.type = EXPR_INT_ATOM;
	atom->value = env->token.intvalue;

	next_token(env);

	return (expression_t*) atom;
}

static
expression_t *parse_expression(parser_env_t *env)
{
	if(env->token.type == T_INTEGER) {
		return parse_int_atom(env);	
	}

	parse_error(env, "Couldn't parse expression");
	expression_t *expr = obstack_alloc(&env->obst, sizeof(expr[0]));
	expr->type = EXPR_INVALID;
	return expr;
}

static
statement_t *parse_return_statement(parser_env_t *env)
{
	return_statement_t *return_statement =
		obstack_alloc(&env->obst, sizeof(return_statement[0]));
	return_statement->statement.type = STATEMENT_RETURN;
	
	next_token(env);
	return_statement->return_value = parse_expression(env);

	return (statement_t*) return_statement;
}

static
statement_t *parse_statement(parser_env_t *env)
{
	switch(env->token.type) {
	case T_return:
		return parse_return_statement(env);
	}

	parse_error(env, "Expected statement");
	eat_until_semi(env);
	statement_t *statement =
		obstack_alloc(&env->obst, sizeof(statement[0]));
	memset(statement, 0, sizeof(statement[0]));
	statement->type = STATEMENT_INVALID;
	return statement;
}

static
statement_t *parse_block(parser_env_t *env)
{
	statement_t       *last = NULL;
	block_statement_t *block = obstack_alloc(&env->obst, sizeof(block[0]));
	memset(block, 0, sizeof(block[0]));

	block->statement.type = STATEMENT_BLOCK;

	while(env->token.type != '}' && env->token.type != T_EOF) {
		if(env->token.type != ';') {
			statement_t *statement = parse_statement(env);
			if(last != NULL) {
				last->next = statement;
			} else {
				block->first_statement = statement;
			}
			last = statement;
		}
		expect(env, ';');
	}

	return (statement_t*) block;
}

static
environment_entry_t *parse_function_or_var(parser_env_t *env)
{
	type_t *type = parse_type(env);
	symbol_t *identifier;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, NULL, T_IDENTIFIER, 0);
		eat_until_semi(env);
		return NULL;
	}
	identifier = env->token.symbol;
	next_token(env);

	/* is it a function? */
	if(env->token.type == '(') {
		environment_entry_t *entry =
			obstack_alloc(&env->obst, sizeof(entry[0]));
		memset(entry, 0, sizeof(entry[0]));
		entry->symbol = identifier;
		entry->type = ENTRY_FUNCTION;

		function_t *function =
			obstack_alloc(&env->obst, sizeof(function[0]));
		memset(function, 0, sizeof(function[0]));
		entry->function = function;

		function->arguments = NULL;
		function->return_type = type;

		/* TODO: free memory in case of error... */

		next_token(env);
		expect(env, ')');
		expect(env, '{');
		function->statement = parse_block(env);
		expect(env, '}');

		return entry;
	} else {
		/* must be a variable */
		expect(env, ';');

		environment_entry_t *entry =
			obstack_alloc(&env->obst, sizeof(entry[0]));
		memset(entry, 0, sizeof(entry[0]));
		entry->symbol = identifier;
		entry->type = ENTRY_VARIABLE;

		variable_t *variable =
			obstack_alloc(&env->obst, sizeof(variable[0]));
		memset(variable, 0, sizeof(variable[0]));
		entry->variable = variable;

		variable->type = type;

		return entry;
	}
}

static
compilation_unit_t *parse_compilation_unit(parser_env_t *env)
{
	compilation_unit_t *unit = obstack_alloc(&env->obst, sizeof(unit[0]));
	environment_t *environment = & unit->environment;
	memset(unit, 0, sizeof(unit[0]));

	while(1) {
		switch(env->token.type) {
		case T_unsigned:
		case T_signed:
		case T_int:
		case T_char:
		case T_short:
		case T_float:
		case T_double: {
			environment_entry_t *entry = parse_function_or_var(env);

			if(entry != NULL) {
				entry->next = environment->entries;
				environment->entries = entry;
			}
			break;
		}

		case ';':
			break;
		case T_EOF:
			return unit;
		}
	}
}

compilation_unit_t *parse(FILE *in)
{
	parser_env_t env;
	memset(&env, 0, sizeof(env));

	obstack_init(&env.obst);

	symbol_table_init(&env.symbol_table);
	put_known_symbols_into_symbol_table(&env.symbol_table);

	lexer_init(&env.lexer, &env.symbol_table, in);
	next_token(&env);

	compilation_unit_t *unit = parse_compilation_unit(&env);

	lexer_destroy(&env.lexer);
	symbol_table_destroy(&env.symbol_table);

	/* FIXME */
	/* obstack_free(&env.obst, NULL); */

	if(env.error) {
		fprintf(stderr, "Errors happened...\n");
		return NULL;
	} else {
		environment_entry_t *entry = unit->environment.entries;
		while(entry != NULL) {
			printf("Entry: '%s', is a %s\n",
					entry->symbol->string,
					entry->type == ENTRY_FUNCTION ? "function" : "variable");
			entry = entry->next;
		}
	}

	return unit;
}

