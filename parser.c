#include <config.h>

#include "parser_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdarg.h>

#include "symbol_table_t.h"
#include "lexer_t.h"
#include "symbol.h"
#include "type_hash.h"
#include "ast_t.h"
#include "type_t.h"
#include "adt/array.h"
#include "adt/obst.h"
#include "adt/util.h"
#include "adt/error.h"

static expression_parse_function_t    *expression_parsers = NULL;
static parse_statement_function       *statement_parsers  = NULL;
static parse_namespace_entry_function *namespace_parsers  = NULL;

static inline
void parser_found_error(parser_env_t *env)
{
	env->error = 1;
#ifdef ABORT_ON_ERROR
	abort();
#endif
}

void parser_print_error_prefix(parser_env_t *env)
{
	fputs(env->lexer.source_position.input_name, stderr);
	fputc(':', stderr);
	fprintf(stderr, "%d", env->lexer.source_position.linenr);
	fputs(": error: ", stderr);
	parser_found_error(env);
}

static
void parse_error(parser_env_t *env, const char *message)
{
	parser_print_error_prefix(env);
	fprintf(stderr, "parse error: %s\n", message);
}

static
void parse_error_expected(parser_env_t *env, const char *message, ...)
{
	va_list args;
	int first = 1;

	if(message != NULL) {
		parser_print_error_prefix(env);
		fprintf(stderr, "%s\n", message);
	}
	parser_print_error_prefix(env);
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
}

static
void eat_until_newline(parser_env_t *env)
{
	while(env->token.type != T_NEWLINE) {
		next_token(env);
		if(env->token.type == T_EOF)
			return;
	}
	next_token(env);
}

static
void maybe_eat_block(parser_env_t *env)
{
	if(env->token.type != T_INDENT)
		return;
	next_token(env);

	unsigned indent = 1;
	while(indent >= 1) {
		if(env->token.type == T_INDENT) {
			indent++;
		} else if(env->token.type == T_DEDENT) {
			indent--;
		} else if(env->token.type == T_EOF) {
			break;
		}
		next_token(env);
	}
}

#define expect(env,expected) \
	if(UNLIKELY(env->token.type != (expected))) { \
		parse_error_expected(env, NULL, (expected), 0); \
		eat_until_newline(env); \
		return NULL; \
	} \
	next_token(env);




static
type_t *parse_type(parser_env_t *env);

static
void parse_parameter_declaration(parser_env_t *env,
                                 method_parameter_type_t **parameter_types,
                                 method_parameter_t **parameters);

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
	case T_bool:
		next_token(env);
		return ATOMIC_TYPE_BOOL;
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

	atomic_type_t *type = obstack_alloc(type_obst, sizeof(type[0]));
	memset(type, 0, sizeof(type[0]));
	type->type.type = TYPE_ATOMIC;
	type->atype = atype;

	type_t *result = typehash_insert((type_t*) type);
	if(result != (type_t*) type) {
		obstack_free(type_obst, type);
	}

	return result;
}

static
type_t *parse_type_ref(parser_env_t *env)
{
	assert(env->token.type == T_IDENTIFIER);

	type_reference_t *type_ref = obstack_alloc(type_obst, sizeof(type_ref[0]));
	memset(type_ref, 0, sizeof(type_ref[0]));

	type_ref->type.type       = TYPE_REFERENCE;
	type_ref->symbol          = env->token.v.symbol;
	type_ref->source_position = env->lexer.source_position;
	next_token(env);

	return (type_t*) type_ref;
}

static
type_t *parse_method_type(parser_env_t *env)
{
	eat(env, T_func);

	method_type_t *method_type
		= obstack_alloc(type_obst, sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;

	method_type->result_type = parse_type(env);
	
	expect(env, '(');
	parse_parameter_declaration(env, &method_type->parameter_types, NULL);
	expect(env, ')');

	return (type_t*) method_type;
}

static
type_t *parse_type(parser_env_t *env)
{
	type_t *type;

	switch(env->token.type) {
	case T_unsigned:
	case T_signed:
	case T_bool:
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
		type = type_void;
		next_token(env);
		break;
	case T_func:
		type = parse_method_type(env);
		break;
	default:
		parse_error(env, "Invalid type");
		type = type_invalid;
		break;
	}

	while(env->token.type == '*') {
		next_token(env);

		pointer_type_t *pointer_type 
			= obstack_alloc(type_obst, sizeof(pointer_type[0]));
		memset(pointer_type, 0, sizeof(pointer_type[0]));

		pointer_type->type.type = TYPE_POINTER;
		pointer_type->points_to = type;

		type = (type_t*) pointer_type;
	}

	return type;
}




static
expression_t *parse_string_const(parser_env_t *env)
{
	string_const_t *cnst = allocate_ast(sizeof(cnst[0]));
	memset(cnst, 0, sizeof(cnst));

	cnst->expression.type = EXPR_STRING_CONST;
	cnst->value           = env->token.v.string;

	next_token(env);

	return (expression_t*) cnst;
}

static
expression_t *parse_int_const(parser_env_t *env)
{
	int_const_t *cnst = allocate_ast(sizeof(cnst[0]));
	memset(cnst, 0, sizeof(cnst));

	cnst->expression.type = EXPR_INT_CONST;
	cnst->value           = env->token.v.intvalue;

	next_token(env);

	return (expression_t*) cnst;
}

static
type_argument_t *parse_type_argument(parser_env_t *env)
{
	type_argument_t *argument = allocate_ast(sizeof(argument[0]));
	memset(argument, 0, sizeof(argument[0]));

	argument->type = parse_type(env);
	return argument;
}

static
type_argument_t *parse_type_arguments(parser_env_t *env)
{
	type_argument_t *first_argument = parse_type_argument(env);
	type_argument_t *last_argument  = first_argument;

	while(env->token.type == ',') {
		next_token(env);
		type_argument_t *type_argument = parse_type_argument(env);

		last_argument->next = type_argument;
		last_argument       = type_argument;
	}

	return first_argument;
}

static
expression_t *parse_reference(parser_env_t *env)
{
	reference_expression_t *ref = allocate_ast(sizeof(ref[0]));
	memset(ref, 0, sizeof(ref[0]));

	ref->expression.type            = EXPR_REFERENCE;
	ref->symbol                     = env->token.v.symbol;

	next_token(env);

	/* this is ambiguous with the less binary expression... */
	if(env->token.type == T_TYPESTART) {
		next_token(env);
		ref->type_arguments = parse_type_arguments(env);
		expect(env, '>');
	}

	return (expression_t*) ref;
}

static
expression_t *parse_sizeof(parser_env_t *env)
{
	sizeof_expression_t *expression
		= allocate_ast(sizeof(expression[0]));
	memset(expression, 0, sizeof(expression[0]));
	expression->expression.type = EXPR_SIZEOF;

	eat(env, T___sizeof);

	expect(env, '<');
	expression->type = parse_type(env);
	expect(env, '>');

	return (expression_t*) expression;
}

void register_statement_parser(parse_statement_function parser, int token_type)
{
	if(token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(statement_parsers);
	if(token_type >= len) {
		ARR_RESIZE(statement_parsers, token_type + 1);
		memset(& statement_parsers[len], 0,
				(token_type - len) * sizeof(statement_parsers[0]));
	}

	if(statement_parsers[token_type] != NULL) {
		panic("Trying to register multiple statement parsers for 1 token");
	}
	statement_parsers[token_type] = parser;
}

void register_namespace_parser(parse_namespace_entry_function parser,
                               int token_type)
{
	if(token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(namespace_parsers);
	if(token_type >= len) {
		ARR_RESIZE(namespace_parsers, token_type + 1);
		memset(& namespace_parsers[len], 0,
				(token_type - len) * sizeof(namespace_parsers[0]));
	}

	if(namespace_parsers[token_type] != NULL) {
		panic("trying to register multiple namespace parsers for 1 token");
	}
	namespace_parsers[token_type] = parser;
}

static
expression_parse_function_t *get_expression_parser_entry(int token_type)
{
	if(token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(expression_parsers);
	if(token_type >= len) {
		ARR_RESIZE(expression_parsers, token_type + 1);
		memset(& expression_parsers[len], 0,
				(token_type - len) * sizeof(expression_parsers[0]));
	}

	return &expression_parsers[token_type];
}

void register_expression_parser(parse_expression_function parser,
                                int token_type, unsigned precedence)
{
	expression_parse_function_t *entry
		= get_expression_parser_entry(token_type);

	if(entry->parser != NULL) {
		panic("trying to register multiple expression parsers for a token");
	}
	entry->parser     = parser;
	entry->precedence = precedence;
}

void register_expression_infix_parser(parse_expression_infix_function parser,
                                      int token_type, unsigned precedence)
{
	expression_parse_function_t *entry
		= get_expression_parser_entry(token_type);

	if(entry->infix_parser != NULL) {
		panic("trying to register multiple infix expression parsers for a "
		      "token");
	}
	entry->infix_parser     = parser;
	entry->infix_precedence = precedence;
}

static
expression_t *expected_expression_error(parser_env_t *env)
{
	parser_print_error_prefix(env);
	fprintf(stderr, "expected expression, got token ");
	print_token(stderr, & env->token);
	fprintf(stderr, "\n");

	expression_t *expression = allocate_ast(sizeof(expression[0]));
	memset(expression, 0, sizeof(expression[0]));
	expression->type = EXPR_INVALID;
	next_token(env);

	return expression;
}

static
expression_t *parse_primary_expression(parser_env_t *env)
{
	switch(env->token.type) {
	case T_INTEGER:
		return parse_int_const(env);
	case T_STRING_LITERAL:
		return parse_string_const(env);
	case T_IDENTIFIER:
		return parse_reference(env);
	case T___sizeof:
		return parse_sizeof(env);
	default:
		return expected_expression_error(env);
	}
}

static
expression_t *parse_brace_expression(parser_env_t *env, unsigned precedence)
{
	(void) precedence;

	eat(env, '(');

	expression_t *result = parse_expression(env);
	expect(env, ')');

	return result;
}

static
expression_t *parse_cast_expression(parser_env_t *env, unsigned precedence)
{
	eat(env, T_cast);

	unary_expression_t *unary_expression
		= allocate_ast(sizeof(unary_expression[0]));
	unary_expression->expression.type            = EXPR_UNARY;
	unary_expression->type                       = UNEXPR_CAST;
	
	expect(env, '<');
	unary_expression->expression.datatype = parse_type(env);
	expect(env, '>');

	unary_expression->value = parse_sub_expression(env, precedence);

	return (expression_t*) unary_expression;
}

static
expression_t *parse_call_expression(parser_env_t *env, unsigned precedence,
                                    expression_t *expression)
{
	(void) precedence;
	call_expression_t *call = allocate_ast(sizeof(call[0]));
	memset(call, 0, sizeof(call[0]));

	call->expression.type            = EXPR_CALL;
	call->method                     = expression;

	/* parse arguments */
	eat(env, '(');

	if(env->token.type != ')') {
		call_argument_t *last_argument = NULL;

		while(1) {
			call_argument_t *argument
				= allocate_ast(sizeof(argument[0]));
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
expression_t *parse_select_expression(parser_env_t *env, unsigned precedence,
                                      expression_t *compound)
{
	(void) precedence;

	eat(env, '.');

	select_expression_t *select = allocate_ast(sizeof(select[0]));
	memset(select, 0, sizeof(select[0]));

	select->expression.type            = EXPR_SELECT;
	select->compound                   = compound;

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
expression_t *parse_array_expression(parser_env_t *env, unsigned precedence,
                                     expression_t *array_ref)
{
	(void) precedence;

	eat(env, '[');

	array_access_expression_t *array_access
		= allocate_ast(sizeof(array_access[0]));
	memset(array_access, 0, sizeof(array_access[0]));

	array_access->expression.type = EXPR_ARRAY_ACCESS;
	array_access->array_ref       = array_ref;
	array_access->index           = parse_expression(env);

	if(env->token.type != ']') {
		parse_error_expected(env, "Problem while parsing array access",
		                     ']', 0);
		return NULL;
	}
	next_token(env);

	return (expression_t*) array_access;
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_type, unexpression_type) \
static                                                           \
expression_t *parse_##unexpression_type(parser_env_t *env,       \
                                        unsigned precedence)     \
{                                                                \
	eat(env, token_type);                                        \
                                                                 \
	unary_expression_t *unary_expression                         \
		= allocate_ast(sizeof(unary_expression[0]));\
	memset(unary_expression, 0, sizeof(unary_expression[0]));    \
	unary_expression->expression.type = EXPR_UNARY;              \
	unary_expression->type            = unexpression_type;       \
	unary_expression->value           = parse_sub_expression(env, precedence); \
                                                                 \
	return (expression_t*) unary_expression;                     \
}

CREATE_UNARY_EXPRESSION_PARSER('-', UNEXPR_NEGATE);
CREATE_UNARY_EXPRESSION_PARSER('!', UNEXPR_NOT);
CREATE_UNARY_EXPRESSION_PARSER('*', UNEXPR_DEREFERENCE);
CREATE_UNARY_EXPRESSION_PARSER('&', UNEXPR_TAKE_ADDRESS);

#define CREATE_BINEXPR_PARSER(token_type, binexpression_type)    \
static                                                           \
expression_t *parse_##binexpression_type(parser_env_t *env,      \
                                         unsigned precedence,    \
                                         expression_t *left)     \
{                                                                \
	eat(env, token_type);                                        \
                                                                 \
	expression_t *right = parse_sub_expression(env, precedence); \
                                                                 \
	binary_expression_t *binexpr                                 \
		= allocate_ast(sizeof(binexpr[0]));         \
	memset(binexpr, 0, sizeof(binexpr[0]));                      \
	binexpr->expression.type            = EXPR_BINARY;           \
	binexpr->type                       = binexpression_type;    \
	binexpr->left                       = left;                  \
	binexpr->right                      = right;                 \
                                                                 \
	return (expression_t*) binexpr;                              \
}

CREATE_BINEXPR_PARSER('*', BINEXPR_MUL);
CREATE_BINEXPR_PARSER('/', BINEXPR_DIV);
CREATE_BINEXPR_PARSER('+', BINEXPR_ADD);
CREATE_BINEXPR_PARSER('-', BINEXPR_SUB);
CREATE_BINEXPR_PARSER('<', BINEXPR_LESS);
CREATE_BINEXPR_PARSER('>', BINEXPR_GREATER);
CREATE_BINEXPR_PARSER('=', BINEXPR_EQUAL);
CREATE_BINEXPR_PARSER(T_ASSIGN, BINEXPR_ASSIGN);
CREATE_BINEXPR_PARSER(T_SLASHEQUAL, BINEXPR_NOTEQUAL);
CREATE_BINEXPR_PARSER(T_LESSEQUAL, BINEXPR_LESSEQUAL);
CREATE_BINEXPR_PARSER(T_GREATEREQUAL, BINEXPR_GREATEREQUAL);
CREATE_BINEXPR_PARSER('&', BINEXPR_AND);
CREATE_BINEXPR_PARSER('|', BINEXPR_OR);
CREATE_BINEXPR_PARSER('^', BINEXPR_XOR);
CREATE_BINEXPR_PARSER(T_LESSLESS, BINEXPR_SHIFTLEFT);
CREATE_BINEXPR_PARSER(T_GREATERGREATER, BINEXPR_SHIFTRIGHT);

static
void register_expression_parsers()
{
	register_expression_infix_parser(parse_BINEXPR_MUL,       '*', 16);
	register_expression_infix_parser(parse_BINEXPR_DIV,       '/', 16);
	register_expression_infix_parser(parse_BINEXPR_SHIFTLEFT, 
	                           T_LESSLESS, 16);
	register_expression_infix_parser(parse_BINEXPR_SHIFTRIGHT,
	                           T_GREATERGREATER, 16);
	register_expression_infix_parser(parse_BINEXPR_ADD,       '+', 15);
	register_expression_infix_parser(parse_BINEXPR_SUB,       '-', 15);
	register_expression_infix_parser(parse_BINEXPR_LESS,      '<', 14);
	register_expression_infix_parser(parse_BINEXPR_GREATER,   '>', 14);
	register_expression_infix_parser(parse_BINEXPR_LESSEQUAL, T_LESSEQUAL, 14);
	register_expression_infix_parser(parse_BINEXPR_GREATEREQUAL,
	                           T_GREATEREQUAL, 14);
	register_expression_infix_parser(parse_BINEXPR_EQUAL,     '=', 13);
	register_expression_infix_parser(parse_BINEXPR_NOTEQUAL, T_SLASHEQUAL, 13);
	register_expression_infix_parser(parse_BINEXPR_AND,       '&', 12);
	register_expression_infix_parser(parse_BINEXPR_XOR,       '^', 11);
	register_expression_infix_parser(parse_BINEXPR_OR,        '|', 10);
	register_expression_infix_parser(parse_BINEXPR_ASSIGN, T_ASSIGN, 2);

	register_expression_infix_parser(parse_array_expression,  '[', 25);

	register_expression_infix_parser(parse_call_expression,   '(', 30);
	register_expression_infix_parser(parse_select_expression, '.', 30);

	register_expression_parser(parse_UNEXPR_NEGATE,           '-',    25);
	register_expression_parser(parse_UNEXPR_NOT,              '!',    25);
	register_expression_parser(parse_UNEXPR_DEREFERENCE,      '*',    20);
	register_expression_parser(parse_UNEXPR_TAKE_ADDRESS,     '&',    20);
	register_expression_parser(parse_cast_expression,         T_cast,  3);
	register_expression_parser(parse_brace_expression,        '(',     1);
}

expression_t *parse_sub_expression(parser_env_t *env, unsigned precedence)
{
	if(env->token.type < 0) {
		return expected_expression_error(env);
	}

	expression_parse_function_t *parser	
		= & expression_parsers[env->token.type];
	source_position_t  source_position  = env->lexer.source_position;
	expression_t      *left;
	
	if(parser->parser != NULL) {
		left = parser->parser(env, parser->precedence);
	} else {
		left = parse_primary_expression(env);
	}
	if(left != NULL)
		left->source_position = source_position;

	while(1) {
		if(env->token.type < 0) {
			return expected_expression_error(env);
		}

		parser = &expression_parsers[env->token.type];
		if(parser->infix_parser == NULL)
			break;
		if(parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(env, parser->infix_precedence, left);
		if(left != NULL)
			left->source_position = source_position;
	}

	return left;
}

expression_t *parse_expression(parser_env_t *env)
{
	return parse_sub_expression(env, 1);
}





static
statement_t *parse_return_statement(parser_env_t *env)
{
	return_statement_t *return_statement =
		allocate_ast(sizeof(return_statement[0]));
	memset(return_statement, 0, sizeof(return_statement[0]));

	return_statement->statement.type = STATEMENT_RETURN;
	next_token(env);

	if(env->token.type != T_NEWLINE) {
		return_statement->return_value = parse_expression(env);
	}
	expect(env, T_NEWLINE);

	return (statement_t*) return_statement;
}

static
statement_t *parse_goto_statement(parser_env_t *env)
{
	eat(env, T_goto);

	goto_statement_t *goto_statement
		= allocate_ast(sizeof(goto_statement[0]));
	memset(goto_statement, 0, sizeof(goto_statement[0]));
	goto_statement->statement.type = STATEMENT_GOTO;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "problem while parsing goto statement",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
		return NULL;
	}
	goto_statement->label_symbol = env->token.v.symbol;
	next_token(env);

	expect(env, T_NEWLINE);

	return (statement_t*) goto_statement;
}

static
statement_t *parse_label_statement(parser_env_t *env)
{
	eat(env, ':');

	label_statement_t *label = allocate_ast(sizeof(label[0]));
	memset(label, 0, sizeof(label[0]));
	label->statement.type = STATEMENT_LABEL;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "problem while parsing label",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
		return NULL;
	}
	label->symbol = env->token.v.symbol;
	next_token(env);

	expect(env, T_NEWLINE);

	return (statement_t*) label;
}

static
statement_t *parse_if_statement(parser_env_t *env)
{
	eat(env, T_if);

	expression_t *condition = parse_expression(env);
	expect(env, ':');

	statement_t *true_statement  = parse_statement(env);
	statement_t *false_statement = NULL;
	if(env->token.type == T_else) {
		next_token(env);
		expect(env, ':');
		false_statement = parse_statement(env);
	}

	if_statement_t *if_statement
		= allocate_ast(sizeof(if_statement[0]));
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
	reference_expression_t *ref = allocate_ast(sizeof(ref[0]));
	memset(ref, 0, sizeof(ref[0]));
	ref->expression.type = EXPR_REFERENCE;
	ref->symbol          = symbol;

	binary_expression_t *assign = allocate_ast(sizeof(assign[0]));
	memset(assign, 0, sizeof(assign[0]));

	assign->expression.type            = EXPR_BINARY;
	assign->expression.source_position = env->lexer.source_position;
	assign->type                       = BINEXPR_ASSIGN;
	assign->left                       = (expression_t*) ref;
	assign->right                      = parse_expression(env);

	expression_statement_t *expr_statement
		= allocate_ast(sizeof(expr_statement[0]));
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

	eat(env, T_var);

	if(env->token.type == '<') {
		next_token(env);
		type = parse_type(env);
		expect(env, '>');
	}

	while(1) {
		decl = allocate_ast(sizeof(decl[0]));
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
			eat_until_newline(env);
			return NULL;
		}
	}

	expect(env, T_NEWLINE);
	return first_statement;
}

static
statement_t *parse_expression_statement(parser_env_t *env)
{
	expression_statement_t *expression_statement
		= allocate_ast(sizeof(expression_statement[0]));
	memset(expression_statement, 0, sizeof(expression_statement[0]));

	expression_statement->statement.type = STATEMENT_EXPRESSION;
	expression_statement->expression     = parse_expression(env);
	expect(env, T_NEWLINE);

	return (statement_t*) expression_statement;
}

static
statement_t *parse_block(parser_env_t *env);

static
statement_t *parse_newline(parser_env_t *env)
{
	eat(env, T_NEWLINE);

	if(env->token.type == T_INDENT)
		return parse_block(env);

	return NULL;
}

static
void register_statement_parsers()
{
	register_statement_parser(parse_return_statement,     T_return);
	register_statement_parser(parse_if_statement,         T_if);
	register_statement_parser(parse_block,                T_INDENT);
	register_statement_parser(parse_variable_declaration, T_var);
	register_statement_parser(parse_label_statement,      ':');
	register_statement_parser(parse_goto_statement,       T_goto);
	register_statement_parser(parse_newline,              T_NEWLINE);
}

statement_t *parse_statement(parser_env_t *env)
{
	statement_t       *statement;
	source_position_t  source_position = env->lexer.source_position;

	if(env->token.type < 0) {
		/* this shouldn't happen if the lexer is correct... */
		parse_error_expected(env, "problem while parsing statement",
		                     T_DEDENT, 0);
		return NULL;
	}

	parse_statement_function parser = NULL;
	if(env->token.type < ARR_LEN(statement_parsers))
		parser = statement_parsers[env->token.type];

	if(parser != NULL) {
		statement = parser(env);
	} else {
		statement = parse_expression_statement(env);
	}

	if(statement == NULL)
		return NULL;

	statement->source_position = source_position;
	statement_t *next = statement->next;
	while(next != NULL) {
		next->source_position = source_position;
		next                  = next->next;
	}

	return statement;
}

static
statement_t *parse_block(parser_env_t *env)
{
	statement_t       *last = NULL;
	block_statement_t *block = allocate_ast(sizeof(block[0]));
	memset(block, 0, sizeof(block[0]));
	block->statement.type = STATEMENT_BLOCK;

	eat(env, T_INDENT);

	while(env->token.type != T_DEDENT && env->token.type != T_EOF) {
		/* parse statement */
		statement_t *statement = parse_statement(env);
		if(statement == NULL)
			continue;

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

	expect(env, T_DEDENT);

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
					= allocate_ast(sizeof(param_type[0]));
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
					= allocate_ast(sizeof(method_param[0]));
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
type_variable_t *parse_type_parameter(parser_env_t *env)
{
	type_constraint_t *last_constraint = NULL;
	type_variable_t   *type_variable
		= allocate_ast(sizeof(type_variable[0]));
	memset(type_variable, 0, sizeof(type_variable[0]));

	while(1) {
		if(env->token.type != T_IDENTIFIER) {
			parse_error_expected(env, "problem while parsing type parameter",
			                     T_IDENTIFIER, 0);
			eat_until_newline(env);
			return NULL;
		}
		symbol_t *symbol = env->token.v.symbol;
		next_token(env);

		if(env->token.type == T_IDENTIFIER) {
			type_constraint_t *constraint 
				= allocate_ast(sizeof(constraint[0]));
			memset(constraint, 0, sizeof(constraint[0]));

			constraint->typeclass_symbol = symbol;
			if(last_constraint == NULL) {
				type_variable->constraints = constraint;
			} else {
				last_constraint->next = constraint;
			}
			last_constraint = constraint;
		} else {
			type_variable->symbol = symbol;
			break;
		}
	}

	return type_variable;
}

static
type_variable_t *parse_type_parameters(parser_env_t *env)
{
	type_variable_t *first_variable = parse_type_parameter(env);
	type_variable_t *last_variable  = first_variable;

	while(env->token.type == ',') {
		next_token(env);
		type_variable_t *type_variable = parse_type_parameter(env);

		last_variable->next = type_variable;
		last_variable       = type_variable;
	}

	return first_variable;
}

static
namespace_entry_t *parse_method(parser_env_t *env)
{
	eat(env, T_func);

	method_t *method = allocate_ast(sizeof(method[0]));
	memset(method, 0, sizeof(method[0]));
	method->namespace_entry.type = NAMESPACE_ENTRY_METHOD;

	method_type_t *method_type
		= obstack_alloc(type_obst, sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;

	if(env->token.type == T___constructor) {
		method->is_constructor = 1;
		next_token(env);
	}

	method_type->result_type = parse_type(env);
	
	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing function",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
	} else {
		method->symbol = env->token.v.symbol;
		next_token(env);

		if(env->token.type == '<') {
			next_token(env);
			method->type_parameters = parse_type_parameters(env);
			expect(env, '>');
		}

		eat(env, '(');

		parse_parameter_declaration(env, &method_type->parameter_types,
									&method->parameters);

		method->type = method_type;

		expect(env, ')');
		expect(env, ':');
	}

	method->statement = parse_statement(env);

	return (namespace_entry_t*) method;
}

static
namespace_entry_t *parse_global_variable(parser_env_t *env)
{
	eat(env, T_var);

	global_variable_t *variable 
		= allocate_ast(sizeof(variable[0]));
	memset(variable, 0, sizeof(variable[0]));
	variable->namespace_entry.type = NAMESPACE_ENTRY_VARIABLE;

	variable->type = parse_type(env);
	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing global variable",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
		return NULL;
	}
	variable->symbol               = env->token.v.symbol;
	next_token(env);

	expect(env, T_NEWLINE);

	return (namespace_entry_t*) variable;
}

static
namespace_entry_t *parse_extern_variable(parser_env_t *env)
{
	namespace_entry_t *entry = parse_global_variable(env);
	if(entry == NULL)
		return NULL;

	assert(entry->type == NAMESPACE_ENTRY_VARIABLE);
	global_variable_t *variable = (global_variable_t*) entry;
	variable->is_extern = 1;

	return entry;
}

static
namespace_entry_t *parse_extern_method(parser_env_t *env)
{
	eat(env, T_func);

	method_t *method = allocate_ast(sizeof(method[0]));
	memset(method, 0, sizeof(method[0]));
	method->namespace_entry.type = NAMESPACE_ENTRY_METHOD;
	method->is_extern            = 1;

	method_type_t *method_type
		= obstack_alloc(type_obst, sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;

	if(env->token.type == T_STRING_LITERAL) {
		method_type->abi_style = env->token.v.string;
		next_token(env);
	}

	method_type->result_type = parse_type(env);

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing extern declaration",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
		return NULL;
	}
	method->symbol = env->token.v.symbol;
	next_token(env);

	expect(env, '(');
	parse_parameter_declaration(env, &method_type->parameter_types,
	                                 &method->parameters);
	expect(env, ')');
	expect(env, T_NEWLINE);

	method->type = method_type;

	return (namespace_entry_t*) method;
}

static
namespace_entry_t *parse_extern(parser_env_t *env)
{
	eat(env, T_extern);

	if(env->token.type == T_func) {
		return parse_extern_method(env);
	} else if(env->token.type == T_var) {
		return parse_extern_variable(env);
	}

	parse_error_expected(env, "Problem while parsing extern declaration",
	                     T_func, T_var, 0);
	eat_until_newline(env);
	return NULL;
}

static
namespace_entry_t *parse_typealias(parser_env_t *env)
{
	eat(env, T_typealias);

	typealias_t *typealias = allocate_ast(sizeof(typealias[0]));
	memset(typealias, 0, sizeof(typealias[0]));
	typealias->namespace_entry.type = NAMESPACE_ENTRY_TYPEALIAS;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing typealias",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
		return NULL;
	}
	typealias->symbol = env->token.v.symbol;
	next_token(env);

	expect(env, T_ASSIGN);
	typealias->type = parse_type(env);

	expect(env, T_NEWLINE);

	return (namespace_entry_t*) typealias;
}

static
compound_entry_t *parse_compound_entries(parser_env_t *env)
{
	eat(env, T_INDENT);

	compound_entry_t *result     = NULL;
	compound_entry_t *last_entry = NULL;
	while(env->token.type != T_DEDENT && env->token.type != T_EOF) {
		compound_entry_t *entry = allocate_ast(sizeof(entry[0]));
		memset(entry, 0, sizeof(entry[0]));

		entry->type = parse_type(env);
		if(env->token.type != T_IDENTIFIER) {
			parse_error_expected(env, "Problem while parsing compound entry",
								 T_IDENTIFIER, 0);
			eat_until_newline(env);
			continue;
		}
		entry->symbol = env->token.v.symbol;
		next_token(env);

		if(last_entry == NULL) {
			result = entry;
		} else {
			last_entry->next = entry;
		}
		last_entry = entry;

		expect(env, T_NEWLINE);
	}
	next_token(env);

	return result;
}

static
namespace_entry_t *parse_struct(parser_env_t *env)
{
	eat(env, T_struct);

	typealias_t *typealias = allocate_ast(sizeof(typealias[0]));
	memset(typealias, 0, sizeof(typealias[0]));
	typealias->namespace_entry.type = NAMESPACE_ENTRY_TYPEALIAS;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing struct",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
		return NULL;
	}
	typealias->symbol = env->token.v.symbol;
	next_token(env);

	expect(env, ':');
	expect(env, T_NEWLINE);

	compound_type_t *compound_type 
		= allocate_ast(sizeof(compound_type[0]));
	memset(compound_type, 0, sizeof(compound_type[0]));
	typealias->type = (type_t*) compound_type;

	compound_type->type.type = TYPE_COMPOUND;
	compound_type->symbol    = typealias->symbol;

	if(env->token.type == T_INDENT) {
		compound_type->entries = parse_compound_entries(env);
	}

	return (namespace_entry_t*) typealias;
}

static
namespace_entry_t *parse_union(parser_env_t *env)
{
	eat(env, T_union);

	typealias_t *typealias = allocate_ast(sizeof(typealias[0]));
	memset(typealias, 0, sizeof(typealias[0]));
	typealias->namespace_entry.type = NAMESPACE_ENTRY_TYPEALIAS;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing union",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
		return NULL;
	}
	typealias->symbol = env->token.v.symbol;
	next_token(env);

	expect(env, ':');
	expect(env, T_NEWLINE);

	compound_type_t *compound_type 
		= allocate_ast(sizeof(compound_type[0]));
	memset(compound_type, 0, sizeof(compound_type[0]));
	typealias->type = (type_t*) compound_type;

	compound_type->type.type = TYPE_COMPOUND;
	compound_type->symbol    = typealias->symbol;
	compound_type->is_union  = 1;

	if(env->token.type == T_INDENT) {
		compound_type->entries = parse_compound_entries(env);
	}

	return (namespace_entry_t*) typealias;
}

static
typeclass_method_t *parse_typeclass_method(parser_env_t *env)
{
	expect(env, T_func);

	typeclass_method_t *method = allocate_ast(sizeof(method[0]));
	memset(method, 0, sizeof(method[0]));

	method_type_t *method_type 
		= obstack_alloc(type_obst, sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;

	method_type->result_type = parse_type(env);
	
	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing typeclass method",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
		return NULL;
	}

	method->symbol = env->token.v.symbol;
	next_token(env);

	expect(env, '(');
	parse_parameter_declaration(env, &method_type->parameter_types,
	                            &method->parameters);
	expect(env, ')');
	expect(env, T_NEWLINE);

	method->method_type = method_type;

	return method;
}

static
namespace_entry_t *parse_typeclass(parser_env_t *env)
{
	eat(env, T_typeclass);

	typeclass_t *typeclass = allocate_ast(sizeof(typeclass[0]));
	memset(typeclass, 0, sizeof(typeclass[0]));
	typeclass->namespace_entry.type = NAMESPACE_ENTRY_TYPECLASS;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing typeclass",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
	} else {
		typeclass->symbol = env->token.v.symbol;
		next_token(env);

		if(env->token.type == '<') {
			next_token(env);
			typeclass->type_parameters = parse_type_parameters(env);
			expect(env, '>');
		}
		expect(env, ':');
		expect(env, T_NEWLINE);
	}

	if(env->token.type != T_INDENT)
		return (namespace_entry_t*) typeclass;
	next_token(env);

	typeclass_method_t *last_method = NULL;
	while(env->token.type != T_DEDENT) {
		if(env->token.type == T_EOF) {
			parse_error(env, "EOF while parsing typeclass");
			return NULL;
		}

		typeclass_method_t *method = parse_typeclass_method(env);
		method->typeclass          = typeclass;

		if(last_method != NULL) {
			last_method->next = method;
		} else {
			typeclass->methods = method;
		}
		last_method = method;
	}
	next_token(env);

	return (namespace_entry_t*) typeclass;
}

static
typeclass_method_instance_t *parse_typeclass_method_intance(parser_env_t *env)
{
	typeclass_method_instance_t *method_intance
		= allocate_ast(sizeof(method_intance[0]));
	memset(method_intance, 0, sizeof(method_intance[0]));

	if(env->token.type != T_func) {
		parse_error_expected(env, "Problem while parsing typeclass method "
		                     "instance", T_func, 0);
		eat_until_newline(env);
		maybe_eat_block(env);
		return NULL;
	}
	namespace_entry_t *entry = parse_method(env);
	if(entry == NULL)
		return NULL;

	assert(entry->type == NAMESPACE_ENTRY_METHOD);
	method_intance->method = (method_t*) entry;

	return method_intance;
}

static
namespace_entry_t *parse_typeclass_instance(parser_env_t *env)
{
	eat(env, T_instance);

	typeclass_instance_t *instance 
		= allocate_ast(sizeof(instance[0]));
	memset(instance, 0, sizeof(instance[0]));
	instance->namespace_entry.type = NAMESPACE_ENTRY_TYPECLASS_INSTANCE;

	if(env->token.type != T_IDENTIFIER) {
		parse_error_expected(env, "Problem while parsing typeclass instance",
		                     T_IDENTIFIER, 0);
		eat_until_newline(env);
	} else {
		instance->typeclass_symbol = env->token.v.symbol;
		next_token(env);

		expect(env, '<');
		instance->type_arguments = parse_type_arguments(env);
		expect(env, '>');
		
		expect(env, ':');
		expect(env, T_NEWLINE);
	}

	if(env->token.type != T_INDENT)
		return (namespace_entry_t*) instance;
	next_token(env);

	typeclass_method_instance_t *last_method = NULL;
	while(env->token.type != T_DEDENT) {
		if(env->token.type == T_EOF) {
			parse_error(env, "EOF while parsing typeclass instance");
			return NULL;
		}

		typeclass_method_instance_t *method
			= parse_typeclass_method_intance(env);

		if(last_method != NULL) {
			last_method->next = method;
		} else {
			instance->method_instances = method;
		}
		last_method = method;
	}
	next_token(env);
	
	return (namespace_entry_t*) instance;
}

namespace_entry_t *parse_namespace_entry(parser_env_t *env)
{
	namespace_entry_t *entry = NULL;

	if(env->token.type < 0) {
		if(env->token.type == T_EOF)
			return NULL;

		/* this shouldn't happen if the lexer is correct... */
		parse_error_expected(env, "problem while parsing namespace entry",
		                     T_DEDENT, 0);
		return NULL;
	}

	parse_namespace_entry_function parser = NULL;
	if(env->token.type < ARR_LEN(namespace_parsers))
		parser = namespace_parsers[env->token.type];

	if(parser == NULL) {
		parse_error_expected(env, "Couldn't parse compilation unit entry",
		                     T_func, T_var, T_extern, T_struct, T_typeclass,
		                     T_instance, 0);
		eat_until_newline(env);
		maybe_eat_block(env);
		return NULL;
	}

	if(parser != NULL) {
		entry = parser(env);
	}

	return entry;
}

static
namespace_t *parse_namespace(parser_env_t *env)
{
	namespace_t *namespace = allocate_ast(sizeof(namespace[0]));
	memset(namespace, 0, sizeof(namespace[0]));

	while(env->token.type != T_EOF) {
		source_position_t  source_position = env->lexer.source_position;
		namespace_entry_t *entry           = parse_namespace_entry(env);

		if(entry != NULL) {
			entry->next            = namespace->entries;
			entry->source_position = source_position;
			namespace->entries     = entry;
		}
	}

	return namespace;
}

static
namespace_entry_t *skip_namespace_entry(parser_env_t *env)
{
	next_token(env);
	return NULL;
}

static
void register_namespace_parsers()
{
	register_namespace_parser(parse_method,             T_func);
	register_namespace_parser(parse_global_variable,    T_var);
	register_namespace_parser(parse_extern,             T_extern);
	register_namespace_parser(parse_struct,             T_struct);
	register_namespace_parser(parse_union,              T_union);
	register_namespace_parser(parse_typealias,          T_typealias);
	register_namespace_parser(parse_typeclass,          T_typeclass);
	register_namespace_parser(parse_typeclass_instance, T_instance);
	register_namespace_parser(skip_namespace_entry,     T_NEWLINE);
}

namespace_t *parse(FILE *in, const char *input_name)
{
	parser_env_t env;
	memset(&env, 0, sizeof(env));

	lexer_init(&env.lexer, in, input_name);

	next_token(&env);

	namespace_t *namespace = parse_namespace(&env);
	namespace->filename    = input_name;

	lexer_destroy(&env.lexer);

	if(env.error) {
		fprintf(stderr, "Errors happened...\n");
		return NULL;
	}

	return namespace;
}

void init_parser(void)
{
	expression_parsers = NEW_ARR_F(expression_parse_function_t, 0);
	statement_parsers  = NEW_ARR_F(parse_statement_function, 0);
	namespace_parsers  = NEW_ARR_F(parse_namespace_entry_function, 0);

	register_expression_parsers();
	register_statement_parsers();
	register_namespace_parsers();
}

void exit_parser(void)
{
	DEL_ARR_F(namespace_parsers);
	DEL_ARR_F(expression_parsers);
	DEL_ARR_F(statement_parsers);
}

