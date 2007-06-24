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

//#define ABORT_ON_ERROR
//#define PRINT_TOKENS

static expression_parse_function_t    *expression_parsers = NULL;
static parse_statement_function       *statement_parsers  = NULL;
static parse_namespace_entry_function *namespace_parsers  = NULL;
static parse_attribute_function       *attribute_parsers  = NULL;

static int      error = 0;
       token_t  token;
       lexer_t  lexer;

static inline
void *allocate_ast_zero(size_t size)
{
	void *res = allocate_ast(size);
	memset(res, 0, size);
	return res;
}

static inline
void *allocate_type_zero(size_t size)
{
	void *res = obstack_alloc(type_obst, size);
	memset(res, 0, size);
	return res;
}

void next_token(void)
{
	lexer_next_token(&lexer, &token);

#ifdef PRINT_TOKENS
	print_token(stderr, &token);
	fprintf(stderr, "\n");
#endif
}

static inline
void eat(token_type_t type)
{
	assert(token.type == type);
	next_token();
}

static inline
void parser_found_error(void)
{
	error = 1;
#ifdef ABORT_ON_ERROR
	abort();
#endif
}

void parser_print_error_prefix(void)
{
	fputs(lexer.source_position.input_name, stderr);
	fputc(':', stderr);
	fprintf(stderr, "%d", lexer.source_position.linenr);
	fputs(": error: ", stderr);
	parser_found_error();
}

static
void parse_error(const char *message)
{
	parser_print_error_prefix();
	fprintf(stderr, "parse error: %s\n", message);
}

static
void parse_error_expected(const char *message, ...)
{
	va_list args;
	int first = 1;

	if(message != NULL) {
		parser_print_error_prefix();
		fprintf(stderr, "%s\n", message);
	}
	parser_print_error_prefix();
	fputs("Parse error: got ", stderr);
	print_token(stderr, &token);
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
void eat_until_newline(void)
{
	while(token.type != T_NEWLINE) {
		next_token();
		if(token.type == T_EOF)
			return;
	}
	next_token();
}

static
void maybe_eat_block(void)
{
	if(token.type != T_INDENT)
		return;
	next_token();

	unsigned indent = 1;
	while(indent >= 1) {
		if(token.type == T_INDENT) {
			indent++;
		} else if(token.type == T_DEDENT) {
			indent--;
		} else if(token.type == T_EOF) {
			break;
		}
		next_token();
	}
}

#define expect(expected)                                   \
	if(UNLIKELY(token.type != (expected))) {               \
		parse_error_expected(NULL, (expected), 0);         \
		eat_until_newline();                               \
		return NULL;                                       \
	}                                                      \
	next_token();

#define expect_void(expected)                              \
	if(UNLIKELY(token.type != (expected))) {               \
		parse_error_expected(NULL, (expected), 0);         \
		eat_until_newline();                               \
		return;                                            \
	}                                                      \
	next_token();




static
type_t *parse_type();

static
void parse_parameter_declaration(method_parameter_type_t **parameter_types,
                                 method_parameter_t **parameters);

static
atomic_type_type_t parse_unsigned_atomic_type(void)
{
	switch(token.type) {
	case T_byte:
		next_token();
		return ATOMIC_TYPE_UBYTE;
	case T_short:
		next_token();
		return ATOMIC_TYPE_USHORT;
	case T_long:
		next_token();
		if(token.type == T_long) {
			next_token();
			return ATOMIC_TYPE_ULONGLONG;
		}
		return ATOMIC_TYPE_ULONG;
	case T_int:
		next_token();
		return ATOMIC_TYPE_UINT;
	default:
		parse_error_expected("couldn't parse type",	T_byte, T_short, T_int,
		                     T_long, 0);
		return ATOMIC_TYPE_INVALID;
	}
}

static
atomic_type_type_t parse_signed_atomic_type(void)
{
	switch(token.type) {
	case T_bool:
		next_token();
		return ATOMIC_TYPE_BOOL;
	case T_byte:
		next_token();
		return ATOMIC_TYPE_BYTE;
	case T_short:
		next_token();
		return ATOMIC_TYPE_SHORT;
	case T_long:
		next_token();
		if(token.type == T_long) {
			next_token();
			return ATOMIC_TYPE_LONGLONG;
		}
		return ATOMIC_TYPE_LONG;
	case T_int:
		next_token();
		return ATOMIC_TYPE_INT;
	case T_float:
		next_token();
		return ATOMIC_TYPE_FLOAT;
	case T_double:
		next_token();
		return ATOMIC_TYPE_DOUBLE;
	default:
		parse_error_expected("couldn't parse type",	T_byte, T_short, T_int,
		                     T_long, T_float, T_double, 0);
		return ATOMIC_TYPE_INVALID;
	}
}

static
type_t *parse_atomic_type(void
		)
{
	atomic_type_type_t atype;

	switch(token.type) {
	case T_unsigned:
		next_token();
		atype = parse_unsigned_atomic_type();
		break;
	case T_signed:
		next_token();
		/* fallthrough */
	default:
		atype = parse_signed_atomic_type();
		break;
	}

	atomic_type_t *type = allocate_type_zero(sizeof(type[0]));
	type->type.type = TYPE_ATOMIC;
	type->atype = atype;

	type_t *result = typehash_insert((type_t*) type);
	if(result != (type_t*) type) {
		obstack_free(type_obst, type);
	}

	return result;
}

static
type_t *parse_type_ref(void)
{
	assert(token.type == T_IDENTIFIER);

	type_reference_t *type_ref = allocate_type_zero(sizeof(type_ref[0]));

	type_ref->type.type       = TYPE_REFERENCE;
	type_ref->symbol          = token.v.symbol;
	type_ref->source_position = lexer.source_position;
	next_token();

	return (type_t*) type_ref;
}

static
type_t *parse_method_type(void)
{
	eat(T_func);

	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;

	expect('(');
	parse_parameter_declaration(&method_type->parameter_types, NULL);
	expect(')');

	expect(':');
	method_type->result_type = parse_type();

	return (type_t*) method_type;
}

static
type_t *parse_type(void)
{
	type_t *type;

	switch(token.type) {
	case T_unsigned:
	case T_signed:
	case T_bool:
	case T_int:
	case T_byte:
	case T_short:
	case T_float:
	case T_double:
		type = parse_atomic_type();
		break;
	case T_IDENTIFIER:
		type = parse_type_ref();
		break;
	case T_void:
		type = type_void;
		next_token();
		break;
	case T_func:
		type = parse_method_type();
		break;
	default:
		parse_error("Invalid type");
		type = type_invalid;
		break;
	}

	/* parse type modifiers */
	pointer_type_t *pointer_type;
	array_type_t   *array_type;
	while(1) {
		switch(token.type) {
		case '*': {
			next_token();

			pointer_type = allocate_type_zero(sizeof(pointer_type[0]));

			pointer_type->type.type = TYPE_POINTER;
			pointer_type->points_to = type;

			type = (type_t*) pointer_type;
			break;
		}
		case '[': {
			next_token();
			if(token.type != T_INTEGER) {
				parse_error_expected("problem while parsing array type",
				                     T_INTEGER, 0);
				break;
			}
			int size = token.v.intvalue;
			next_token();

			if(size < 0) {
				parse_error("negative array size not allowed");
				expect(']');
				break;
			}

			array_type = allocate_type_zero(sizeof(array_type[0]));

			array_type->type.type    = TYPE_ARRAY;
			array_type->element_type = type;
			array_type->size         = size;

			type = (type_t*) array_type;

			expect(']');
			break;
			}
		default:
			return type;
		}
	}
}




static
expression_t *parse_string_const(void)
{
	string_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_STRING_CONST;
	cnst->value           = token.v.string;

	next_token();

	return (expression_t*) cnst;
}

static
expression_t *parse_int_const(void)
{
	int_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_INT_CONST;
	cnst->value           = token.v.intvalue;

	next_token();

	return (expression_t*) cnst;
}

static
type_argument_t *parse_type_argument(void)
{
	type_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));

	argument->type = parse_type();
	return argument;
}

static
type_argument_t *parse_type_arguments(void)
{
	type_argument_t *first_argument = parse_type_argument();
	type_argument_t *last_argument  = first_argument;

	while(token.type == ',') {
		next_token();
		type_argument_t *type_argument = parse_type_argument();

		last_argument->next = type_argument;
		last_argument       = type_argument;
	}

	return first_argument;
}

static
expression_t *parse_reference(void)
{
	reference_expression_t *ref = allocate_ast_zero(sizeof(ref[0]));

	ref->expression.type            = EXPR_REFERENCE;
	ref->symbol                     = token.v.symbol;

	next_token();

	if(token.type == T_TYPESTART) {
		next_token();
		ref->type_arguments = parse_type_arguments();
		expect('>');
	}

	return (expression_t*) ref;
}

static
expression_t *parse_sizeof(void)
{
	eat(T___sizeof);

	sizeof_expression_t *expression	= allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type = EXPR_SIZEOF;

	expect('<');
	expression->type = parse_type();
	expect('>');

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
				(token_type - len + 1) * sizeof(statement_parsers[0]));
	}

	if(statement_parsers[token_type] != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
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
				(token_type - len + 1) * sizeof(namespace_parsers[0]));
	}

	if(namespace_parsers[token_type] != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple namespace parsers for 1 token");
	}
	namespace_parsers[token_type] = parser;
}

void register_attribute_parser(parse_attribute_function parser, int token_type)
{
	if(token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(attribute_parsers);
	if(token_type >= len) {
		ARR_RESIZE(attribute_parsers, token_type + 1);
		memset(& attribute_parsers[len], 0,
				(token_type - len + 1) * sizeof(attribute_parsers[0]));
	}

	if(attribute_parsers[token_type] != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple namespace parsers for 1 token");
	}
	attribute_parsers[token_type] = parser;
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
				(token_type - len + 1) * sizeof(expression_parsers[0]));
	}

	return &expression_parsers[token_type];
}

void register_expression_parser(parse_expression_function parser,
                                int token_type, unsigned precedence)
{
	expression_parse_function_t *entry
		= get_expression_parser_entry(token_type);

	if(entry->parser != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
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
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple infix expression parsers for a "
		      "token");
	}
	entry->infix_parser     = parser;
	entry->infix_precedence = precedence;
}

static
expression_t *expected_expression_error(void)
{
	parser_print_error_prefix();
	fprintf(stderr, "expected expression, got token ");
	print_token(stderr, & token);
	fprintf(stderr, "\n");

	expression_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->type = EXPR_INVALID;
	next_token();

	return expression;
}

static
expression_t *parse_primary_expression(void)
{
	switch(token.type) {
	case T_INTEGER:
		return parse_int_const();
	case T_STRING_LITERAL:
		return parse_string_const();
	case T_IDENTIFIER:
		return parse_reference();
	case T___sizeof:
		return parse_sizeof();
	default:
		return expected_expression_error();
	}
}

static
expression_t *parse_brace_expression(unsigned precedence)
{
	(void) precedence;

	eat('(');

	expression_t *result = parse_expression();
	expect(')');

	return result;
}

static
expression_t *parse_cast_expression(unsigned precedence)
{
	eat(T_cast);

	unary_expression_t *unary_expression 
		= allocate_ast_zero(sizeof(unary_expression[0]));
	unary_expression->expression.type            = EXPR_UNARY;
	unary_expression->type                       = UNEXPR_CAST;
	
	expect('<');
	unary_expression->expression.datatype = parse_type();
	expect('>');

	unary_expression->value = parse_sub_expression(precedence);

	return (expression_t*) unary_expression;
}

static
expression_t *parse_call_expression(unsigned precedence,
                                    expression_t *expression)
{
	(void) precedence;
	call_expression_t *call = allocate_ast_zero(sizeof(call[0]));

	call->expression.type            = EXPR_CALL;
	call->method                     = expression;

	/* parse arguments */
	eat('(');

	if(token.type != ')') {
		call_argument_t *last_argument = NULL;

		while(1) {
			call_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));

			argument->expression = parse_expression();
			if(last_argument == NULL) {
				call->arguments = argument;
			} else {
				last_argument->next = argument;
			}
			last_argument = argument;

			if(token.type != ',')
				break;
			next_token();
		}
	}
	expect(')');

	return (expression_t*) call;
}

static
expression_t *parse_select_expression(unsigned precedence,
                                      expression_t *compound)
{
	(void) precedence;

	eat('.');

	select_expression_t *select = allocate_ast_zero(sizeof(select[0]));

	select->expression.type            = EXPR_SELECT;
	select->compound                   = compound;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing compound select",
		                     T_IDENTIFIER, 0);
		return NULL;
	}
	select->symbol          = token.v.symbol;
	next_token();

	return (expression_t*) select;
}

static
expression_t *parse_array_expression(unsigned precedence,
                                     expression_t *array_ref)
{
	(void) precedence;

	eat('[');

	array_access_expression_t *array_access
		= allocate_ast_zero(sizeof(array_access[0]));

	array_access->expression.type = EXPR_ARRAY_ACCESS;
	array_access->array_ref       = array_ref;
	array_access->index           = parse_expression();

	if(token.type != ']') {
		parse_error_expected("Problem while parsing array access", ']', 0);
		return NULL;
	}
	next_token();

	return (expression_t*) array_access;
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_type, unexpression_type)     \
static                                                                    \
expression_t *parse_##unexpression_type(unsigned precedence)              \
{                                                                         \
	eat(token_type);                                                      \
                                                                          \
	unary_expression_t *unary_expression                                  \
		= allocate_ast_zero(sizeof(unary_expression[0]));                 \
	unary_expression->expression.type = EXPR_UNARY;                       \
	unary_expression->type            = unexpression_type;                \
	unary_expression->value           = parse_sub_expression(precedence); \
                                                                          \
	return (expression_t*) unary_expression;                              \
}

CREATE_UNARY_EXPRESSION_PARSER('-', UNEXPR_NEGATE);
CREATE_UNARY_EXPRESSION_PARSER('!', UNEXPR_NOT);
CREATE_UNARY_EXPRESSION_PARSER('*', UNEXPR_DEREFERENCE);
CREATE_UNARY_EXPRESSION_PARSER('&', UNEXPR_TAKE_ADDRESS);

#define CREATE_BINEXPR_PARSER(token_type, binexpression_type)    \
static                                                           \
expression_t *parse_##binexpression_type(unsigned precedence,    \
                                         expression_t *left)     \
{                                                                \
	eat(token_type);                                             \
                                                                 \
	expression_t *right = parse_sub_expression(precedence);      \
                                                                 \
	binary_expression_t *binexpr                                 \
		= allocate_ast_zero(sizeof(binexpr[0]));                 \
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
void register_expression_parsers(void)
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

expression_t *parse_sub_expression(unsigned precedence)
{
	if(token.type < 0) {
		return expected_expression_error();
	}

	expression_parse_function_t *parser	
		= & expression_parsers[token.type];
	source_position_t  source_position  = lexer.source_position;
	expression_t      *left;
	
	if(parser->parser != NULL) {
		left = parser->parser(parser->precedence);
	} else {
		left = parse_primary_expression();
	}
	if(left != NULL)
		left->source_position = source_position;

	while(1) {
		if(token.type < 0) {
			return expected_expression_error();
		}

		parser = &expression_parsers[token.type];
		if(parser->infix_parser == NULL)
			break;
		if(parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(parser->infix_precedence, left);
		if(left != NULL)
			left->source_position = source_position;
	}

	return left;
}

expression_t *parse_expression(void)
{
	return parse_sub_expression(1);
}





static
statement_t *parse_return_statement(void)
{
	return_statement_t *return_statement =
		allocate_ast_zero(sizeof(return_statement[0]));

	return_statement->statement.type = STATEMENT_RETURN;
	next_token();

	if(token.type != T_NEWLINE) {
		return_statement->return_value = parse_expression();
	}
	expect(T_NEWLINE);

	return (statement_t*) return_statement;
}

static
statement_t *parse_goto_statement(void)
{
	eat(T_goto);

	goto_statement_t *goto_statement
		= allocate_ast_zero(sizeof(goto_statement[0]));
	goto_statement->statement.type = STATEMENT_GOTO;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing goto statement",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	goto_statement->label_symbol = token.v.symbol;
	next_token();

	expect(T_NEWLINE);

	return (statement_t*) goto_statement;
}

static
statement_t *parse_label_statement(void)
{
	eat(':');

	label_statement_t *label = allocate_ast_zero(sizeof(label[0]));
	label->statement.type = STATEMENT_LABEL;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing label", T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	label->symbol = token.v.symbol;
	next_token();

	expect(T_NEWLINE);

	return (statement_t*) label;
}

static
statement_t *parse_if_statement(void)
{
	eat(T_if);

	expression_t *condition = parse_expression();
	expect(':');

	statement_t *true_statement  = parse_statement();
	statement_t *false_statement = NULL;
	if(token.type == T_else) {
		next_token();
		expect(':');
		false_statement = parse_statement();
	}

	if_statement_t *if_statement
		= allocate_ast_zero(sizeof(if_statement[0]));

	if_statement->statement.type  = STATEMENT_IF;
	if_statement->condition       = condition;
	if_statement->true_statement  = true_statement;
	if_statement->false_statement = false_statement;

	return (statement_t*) if_statement;
}

static
statement_t *parse_initial_assignment(symbol_t *symbol)
{
	reference_expression_t *ref = allocate_ast_zero(sizeof(ref[0]));
	ref->expression.type = EXPR_REFERENCE;
	ref->symbol          = symbol;

	binary_expression_t *assign = allocate_ast_zero(sizeof(assign[0]));

	assign->expression.type            = EXPR_BINARY;
	assign->expression.source_position = lexer.source_position;
	assign->type                       = BINEXPR_ASSIGN;
	assign->left                       = (expression_t*) ref;
	assign->right                      = parse_expression();

	expression_statement_t *expr_statement
		= allocate_ast_zero(sizeof(expr_statement[0]));

	expr_statement->statement.type = STATEMENT_EXPRESSION;
	expr_statement->expression     = (expression_t*) assign;

	return (statement_t*) expr_statement;
}

static
statement_t *parse_variable_declaration(void)
{
	statement_t                      *first_statement = NULL;
	statement_t                      *last_statement  = NULL;
	variable_declaration_statement_t *decl;
	type_t                           *type            = NULL;

	eat(T_var);

	while(1) {
		if(token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing variable declaration",
			                     T_IDENTIFIER, 0);
			eat_until_newline();
			return NULL;
		}

		decl = allocate_ast_zero(sizeof(decl[0]));
		decl->statement.type = STATEMENT_VARIABLE_DECLARATION;
		decl->type           = type;
		decl->symbol         = token.v.symbol;
		next_token();

		if(token.type == ':') {
			next_token();
			decl->type = parse_type();
		}

		/* append multiple variable declarations */
		if(last_statement != NULL) {
			last_statement->next = (statement_t*) decl;
		} else {
			first_statement = (statement_t*) decl;
		}
		last_statement = (statement_t*) decl;

		/* do we have an assignment expression? */
		if(token.type == T_ASSIGN) {
			next_token();
			statement_t *assign = parse_initial_assignment(decl->symbol);

			last_statement->next = assign;
			last_statement = assign;
		}

		/* check if we have more declared symbols separated by ',' */
		if(token.type != ',')
			break;
		next_token();
	}

	expect(T_NEWLINE);
	return first_statement;
}

static
statement_t *parse_expression_statement(void)
{
	expression_statement_t *expression_statement
		= allocate_ast_zero(sizeof(expression_statement[0]));

	expression_statement->statement.type = STATEMENT_EXPRESSION;
	expression_statement->expression     = parse_expression();
	expect(T_NEWLINE);

	return (statement_t*) expression_statement;
}

static
statement_t *parse_block(void);

static
statement_t *parse_newline(void)
{
	eat(T_NEWLINE);

	if(token.type == T_INDENT)
		return parse_block();

	return NULL;
}

static
void register_statement_parsers(void)
{
	register_statement_parser(parse_return_statement,     T_return);
	register_statement_parser(parse_if_statement,         T_if);
	register_statement_parser(parse_block,                T_INDENT);
	register_statement_parser(parse_variable_declaration, T_var);
	register_statement_parser(parse_label_statement,      ':');
	register_statement_parser(parse_goto_statement,       T_goto);
	register_statement_parser(parse_newline,              T_NEWLINE);
}

statement_t *parse_statement(void)
{
	statement_t       *statement;
	source_position_t  source_position = lexer.source_position;

	if(token.type < 0) {
		/* this shouldn't happen if the lexer is correct... */
		parse_error_expected("problem while parsing statement",
		                     T_DEDENT, 0);
		return NULL;
	}

	parse_statement_function parser = NULL;
	if(token.type < ARR_LEN(statement_parsers))
		parser = statement_parsers[token.type];

	if(parser != NULL) {
		statement = parser();
	} else {
		statement = parse_expression_statement();
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
statement_t *parse_block(void)
{
	statement_t       *last = NULL;
	block_statement_t *block = allocate_ast_zero(sizeof(block[0]));
	block->statement.type = STATEMENT_BLOCK;

	eat(T_INDENT);

	while(token.type != T_DEDENT && token.type != T_EOF) {
		/* parse statement */
		statement_t *statement = parse_statement();
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

	expect(T_DEDENT);

	return (statement_t*) block;
}

static
void parse_parameter_declaration(method_parameter_type_t **parameter_types,
                                 method_parameter_t **parameters)
{
	if(token.type == ')')
		return;

	method_parameter_type_t *last_type = NULL;
	method_parameter_t      *last_param = NULL;
	if(parameter_types != NULL)
		*parameter_types = NULL;
	if(parameters != NULL)
		*parameters = NULL;

	while(1) {
		if(token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing parameter",
			                     T_IDENTIFIER, 0);
			eat_until_newline();
			return;
		}
		symbol_t *symbol = token.v.symbol;
		next_token();

		expect_void(':');
		type_t *type = parse_type();

		if(parameter_types != NULL) {
			method_parameter_type_t *param_type
				= allocate_ast_zero(sizeof(param_type[0]));
			param_type->type = type;

			if(last_type != NULL) {
				last_type->next = param_type;
			} else {
				*parameter_types = param_type;
			}
			last_type = param_type;
		}

		if(parameters != NULL) {
			method_parameter_t *method_param
				= allocate_ast_zero(sizeof(method_param[0]));
			method_param->symbol = symbol;
			method_param->type   = type;

			if(last_param != NULL) {
				last_param->next = method_param;
			} else {
				*parameters = method_param;
			}
			last_param = method_param;
		}

		if(token.type != ',')
			break;
		next_token();
	}
}

static
type_variable_t *parse_type_parameter(void)
{
	type_constraint_t *last_constraint = NULL;
	type_variable_t   *type_variable
		= allocate_ast_zero(sizeof(type_variable[0]));

	while(1) {
		if(token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing type parameter",
			                     T_IDENTIFIER, 0);
			eat_until_newline();
			return NULL;
		}
		symbol_t *symbol = token.v.symbol;
		next_token();

		if(token.type == T_IDENTIFIER) {
			type_constraint_t *constraint 
				= allocate_ast_zero(sizeof(constraint[0]));

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
type_variable_t *parse_type_parameters(void)
{
	type_variable_t *first_variable = parse_type_parameter();
	type_variable_t *last_variable  = first_variable;

	while(token.type == ',') {
		next_token();
		type_variable_t *type_variable = parse_type_parameter();

		last_variable->next = type_variable;
		last_variable       = type_variable;
	}

	return first_variable;
}

static
namespace_entry_t *parse_method(void)
{
	eat(T_func);

	method_t *method = allocate_ast_zero(sizeof(method[0]));
	method->namespace_entry.type = NAMESPACE_ENTRY_METHOD;

	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;

	if(token.type == T___constructor) {
		method->is_constructor = 1;
		next_token();
	}

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing function",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
	} else {
		method->symbol = token.v.symbol;
		next_token();

		if(token.type == '<') {
			next_token();
			method->type_parameters = parse_type_parameters();
			expect('>');
		}

		eat('(');

		parse_parameter_declaration(&method_type->parameter_types,
									&method->parameters);

		method->type = method_type;

		expect(')');
		expect(':');

		if(token.type != T_NEWLINE) {
			method_type->result_type = parse_type();
			expect(':');
		} else {
			method_type->result_type = type_void;
		}
	}

	method->statement = parse_statement();

	return (namespace_entry_t*) method;
}

static
namespace_entry_t *parse_global_variable(void)
{
	eat(T_var);

	global_variable_t *variable 
		= allocate_ast_zero(sizeof(variable[0]));
	variable->namespace_entry.type = NAMESPACE_ENTRY_VARIABLE;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing global variable",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	variable->symbol               = token.v.symbol;
	next_token();

	expect(':');
	variable->type = parse_type();

	expect(T_NEWLINE);

	return (namespace_entry_t*) variable;
}

static
namespace_entry_t *parse_constant(void)
{
	eat(T_const);

	constant_t *constant = allocate_ast_zero(sizeof(constant[0]));
	constant->namespace_entry.type = NAMESPACE_ENTRY_CONSTANT;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing constant", T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	constant->symbol = token.v.symbol;
	next_token();

	if(token.type == '<') {
		next_token();
		constant->type = parse_type();
		expect('>');
	}

	expect(T_ASSIGN);
	constant->expression = parse_expression();

	expect(T_NEWLINE);

	return (namespace_entry_t*) constant;
}

static
namespace_entry_t *parse_extern_variable(void)
{
	namespace_entry_t *entry = parse_global_variable();
	if(entry == NULL)
		return NULL;

	assert(entry->type == NAMESPACE_ENTRY_VARIABLE);
	global_variable_t *variable = (global_variable_t*) entry;
	variable->is_extern = 1;

	return entry;
}

static
namespace_entry_t *parse_extern_method(void)
{
	eat(T_func);

	method_t *method = allocate_ast_zero(sizeof(method[0]));
	method->namespace_entry.type = NAMESPACE_ENTRY_METHOD;
	method->is_extern            = 1;

	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;

	if(token.type == T_STRING_LITERAL) {
		method_type->abi_style = token.v.string;
		next_token();
	}

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing extern declaration",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	method->symbol = token.v.symbol;
	next_token();

	expect('(');
	parse_parameter_declaration(&method_type->parameter_types,
	                            &method->parameters);
	expect(')');

	if(token.type == ':') {
		next_token();
		method_type->result_type = parse_type();
	} else {
		method_type->result_type = type_void;
	}

	expect(T_NEWLINE);

	method->type = method_type;

	return (namespace_entry_t*) method;
}

static
namespace_entry_t *parse_extern(void)
{
	eat(T_extern);

	if(token.type == T_func) {
		return parse_extern_method();
	} else if(token.type == T_var) {
		return parse_extern_variable();
	}

	parse_error_expected("Problem while parsing extern declaration",
	                     T_func, T_var, 0);
	eat_until_newline();
	return NULL;
}

static
namespace_entry_t *parse_typealias(void)
{
	eat(T_typealias);

	typealias_t *typealias = allocate_ast_zero(sizeof(typealias[0]));
	typealias->namespace_entry.type = NAMESPACE_ENTRY_TYPEALIAS;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typealias",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	typealias->symbol = token.v.symbol;
	next_token();

	expect(T_ASSIGN);
	typealias->type = parse_type();

	expect(T_NEWLINE);

	return (namespace_entry_t*) typealias;
}

static
attribute_t *parse_attribute(void)
{
	eat('$');

	attribute_t *attribute = NULL;

	if(token.type < 0) {
		parse_error("problem while parsing attribute");
		return NULL;
	}

	parse_attribute_function parser = NULL;
	if(token.type < ARR_LEN(attribute_parsers))
		parser = attribute_parsers[token.type];

	if(parser == NULL) {
		parser_print_error_prefix();
		print_token(stderr, &token);
		fprintf(stderr, " doesn't start a known attribute type\n");
		return NULL;
	}

	if(parser != NULL) {
		attribute = parser();
	}

	return attribute;
}

attribute_t *parse_attributes(void)
{
	attribute_t *last = NULL;

	while(token.type == '$') {
		attribute_t *attribute = parse_attribute();
		if(attribute != NULL) {
			attribute->next = last;
			last = attribute;
		}
	}

	return last;
}

static
compound_entry_t *parse_compound_entries(void)
{
	eat(T_INDENT);

	compound_entry_t *result     = NULL;
	compound_entry_t *last_entry = NULL;
	while(token.type != T_DEDENT && token.type != T_EOF) {
		compound_entry_t *entry = allocate_ast_zero(sizeof(entry[0]));

		if(token.type != T_IDENTIFIER) {
			parse_error_expected("Problem while parsing compound entry",
								 T_IDENTIFIER, 0);
			eat_until_newline();
			continue;
		}
		entry->symbol = token.v.symbol;
		next_token();

		expect(':');
		entry->type       = parse_type();
		entry->attributes = parse_attributes();

		if(last_entry == NULL) {
			result = entry;
		} else {
			last_entry->next = entry;
		}
		last_entry = entry;

		expect(T_NEWLINE);
	}
	next_token();

	return result;
}

static
namespace_entry_t *parse_struct(void)
{
	eat(T_struct);

	typealias_t *typealias = allocate_ast_zero(sizeof(typealias[0]));
	typealias->namespace_entry.type = NAMESPACE_ENTRY_TYPEALIAS;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing struct",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	typealias->symbol = token.v.symbol;
	next_token();

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_STRUCT;
	compound_type->symbol     = typealias->symbol;
	compound_type->attributes = parse_attributes();

	typealias->type = (type_t*) compound_type;

	expect(':');
	expect(T_NEWLINE);

	if(token.type == T_INDENT) {
		compound_type->entries = parse_compound_entries();
	}

	return (namespace_entry_t*) typealias;
}

static
namespace_entry_t *parse_union(void)
{
	eat(T_union);

	typealias_t *typealias = allocate_ast_zero(sizeof(typealias[0]));
	typealias->namespace_entry.type = NAMESPACE_ENTRY_TYPEALIAS;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing union",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	typealias->symbol = token.v.symbol;
	next_token();

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_UNION;
	compound_type->symbol     = typealias->symbol;
	compound_type->attributes = parse_attributes();

	typealias->type = (type_t*) compound_type;

	expect(':');
	expect(T_NEWLINE);

	if(token.type == T_INDENT) {
		compound_type->entries = parse_compound_entries();
	}

	return (namespace_entry_t*) typealias;
}

static
typeclass_method_t *parse_typeclass_method(void)
{
	eat(T_func);

	typeclass_method_t *method = allocate_ast_zero(sizeof(method[0]));

	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;
	
	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typeclass method",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}

	method->symbol = token.v.symbol;
	next_token();

	expect('(');
	parse_parameter_declaration(&method_type->parameter_types,
	                            &method->parameters);
	expect(')');

	if(token.type == ':') {
		next_token();
		method_type->result_type = parse_type();
	} else {
		method_type->result_type = type_void;
	}
	expect(T_NEWLINE);

	method->method_type = method_type;

	return method;
}

static
namespace_entry_t *parse_typeclass(void)
{
	eat(T_typeclass);

	typeclass_t *typeclass = allocate_ast_zero(sizeof(typeclass[0]));
	typeclass->namespace_entry.type = NAMESPACE_ENTRY_TYPECLASS;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typeclass",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
	} else {
		typeclass->symbol = token.v.symbol;
		next_token();

		if(token.type == '<') {
			next_token();
			typeclass->type_parameters = parse_type_parameters();
			expect('>');
		}
		expect(':');
		expect(T_NEWLINE);
	}

	if(token.type != T_INDENT)
		return (namespace_entry_t*) typeclass;
	next_token();

	typeclass_method_t *last_method = NULL;
	while(token.type != T_DEDENT) {
		if(token.type == T_EOF) {
			parse_error("EOF while parsing typeclass");
			return NULL;
		}

		typeclass_method_t *method = parse_typeclass_method();
		method->typeclass          = typeclass;

		if(last_method != NULL) {
			last_method->next = method;
		} else {
			typeclass->methods = method;
		}
		last_method = method;
	}
	next_token();

	return (namespace_entry_t*) typeclass;
}

static
typeclass_method_instance_t *parse_typeclass_method_instance(void)
{
	typeclass_method_instance_t *method_intance
		= allocate_ast_zero(sizeof(method_intance[0]));

	if(token.type != T_func) {
		parse_error_expected("Problem while parsing typeclass method "
		                     "instance", T_func, 0);
		eat_until_newline();
		maybe_eat_block();
		return NULL;
	}
	namespace_entry_t *entry = parse_method();
	if(entry == NULL)
		return NULL;

	assert(entry->type == NAMESPACE_ENTRY_METHOD);
	method_intance->method = (method_t*) entry;

	return method_intance;
}

static
namespace_entry_t *parse_typeclass_instance(void)
{
	eat(T_instance);

	typeclass_instance_t *instance 
		= allocate_ast_zero(sizeof(instance[0]));
	instance->namespace_entry.type = NAMESPACE_ENTRY_TYPECLASS_INSTANCE;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typeclass instance",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
	} else {
		instance->typeclass_symbol = token.v.symbol;
		next_token();

		instance->type_arguments = parse_type_arguments();
		
		expect(':');
		expect(T_NEWLINE);
	}

	if(token.type != T_INDENT)
		return (namespace_entry_t*) instance;
	next_token();

	typeclass_method_instance_t *last_method = NULL;
	while(token.type != T_DEDENT) {
		if(token.type == T_EOF) {
			parse_error("EOF while parsing typeclass instance");
			return NULL;
		}

		typeclass_method_instance_t *method	= parse_typeclass_method_instance();

		if(last_method != NULL) {
			last_method->next = method;
		} else {
			instance->method_instances = method;
		}
		last_method = method;
	}
	next_token();
	
	return (namespace_entry_t*) instance;
}

namespace_entry_t *parse_namespace_entry(void)
{
	namespace_entry_t *entry = NULL;

	if(token.type < 0) {
		if(token.type == T_EOF)
			return NULL;

		/* this shouldn't happen if the lexer is correct... */
		parse_error_expected("problem while parsing namespace entry",
		                     T_DEDENT, 0);
		return NULL;
	}

	parse_namespace_entry_function parser = NULL;
	if(token.type < ARR_LEN(namespace_parsers))
		parser = namespace_parsers[token.type];

	if(parser == NULL) {
		parse_error_expected("Couldn't parse compilation unit entry",
		                     T_func, T_var, T_extern, T_struct, T_typeclass,
		                     T_instance, 0);
		eat_until_newline();
		maybe_eat_block();
		return NULL;
	}

	if(parser != NULL) {
		entry = parser();
	}

	return entry;
}

static
namespace_t *get_namespace(symbol_t *symbol)
{
	/* search for an existing namespace */
	namespace_t *namespace = namespaces;
	while(namespace != NULL) {
		if(namespace->symbol == symbol)
			return namespace;
	
		namespace = namespace->next;
	}

	namespace = allocate_ast_zero(sizeof(namespace[0]));
	namespace->symbol = symbol;

	namespace->next = namespaces;
	namespaces      = namespace;

	return namespace;
}

static
namespace_t *parse_namespace(void)
{
	symbol_t *namespace_symbol = NULL;

	/* parse namespace name */
	if(token.type == T_namespace) {
		next_token();
		if(token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing namespace", 
			                     T_IDENTIFIER, 0);
			eat_until_newline();
		}
		namespace_symbol = token.v.symbol;
		next_token();

		if(token.type != T_NEWLINE) {
			parse_error("extra tokens after namespace definition");
			eat_until_newline();
		} else {
			next_token();
		}
	}
		
	namespace_t *namespace = get_namespace(namespace_symbol);

	/* parse namespace entries */
	while(token.type != T_EOF) {
		source_position_t  source_position = lexer.source_position;
		namespace_entry_t *entry           = parse_namespace_entry();

		if(entry != NULL) {
			entry->next            = namespace->entries;
			entry->source_position = source_position;
			namespace->entries     = entry;
		}
	}

	return namespace;
}

static
namespace_entry_t *skip_namespace_entry(void)
{
	next_token();
	return NULL;
}

static
void register_namespace_parsers(void)
{
	register_namespace_parser(parse_method,             T_func);
	register_namespace_parser(parse_global_variable,    T_var);
	register_namespace_parser(parse_constant,           T_const);
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
	lexer_init(&lexer, in, input_name);

	next_token();

	namespace_t *namespace = parse_namespace();
	namespace->filename    = input_name;

	lexer_destroy(&lexer);

	if(error) {
		fprintf(stderr, "syntax errors found...\n");
		return NULL;
	}

	return namespace;
}

void init_parser(void)
{
	expression_parsers = NEW_ARR_F(expression_parse_function_t, 0);
	statement_parsers  = NEW_ARR_F(parse_statement_function, 0);
	namespace_parsers  = NEW_ARR_F(parse_namespace_entry_function, 0);
	attribute_parsers  = NEW_ARR_F(parse_attribute_function, 0);

	register_expression_parsers();
	register_statement_parsers();
	register_namespace_parsers();
}

void exit_parser(void)
{
	DEL_ARR_F(attribute_parsers);
	DEL_ARR_F(namespace_parsers);
	DEL_ARR_F(expression_parsers);
	DEL_ARR_F(statement_parsers);
}

