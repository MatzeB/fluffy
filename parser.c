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

static expression_parse_function_t *expression_parsers = NULL;
static parse_statement_function    *statement_parsers  = NULL;
static parse_declaration_function  *declaration_parsers  = NULL;
static parse_attribute_function    *attribute_parsers  = NULL;

static context_t *current_context = NULL;

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

/**
 * error recovery: skip a block and all contained sub-blocks
 */
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

/**
 * error recovery: try to got to the next line. If the current line ends in ':'
 * then we skip blocks that might follow
 */
static
void eat_until_newline(void)
{
	int prev = -1;

	while(token.type != T_NEWLINE) {
		prev = token.type;
		next_token();
		if(token.type == T_EOF)
			return;
	}
	next_token();

	if(prev == ':') {
		maybe_eat_block();
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
void parse_method(method_t *method);

static
statement_t *parse_block(void);

static
void parse_parameter_declaration(method_type_t *method_type,
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
type_t *parse_atomic_type(void)
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
	parse_parameter_declaration(method_type, NULL);
	expect(')');

	expect(':');
	method_type->result_type = parse_type();

	return (type_t*) method_type;
}

static
type_t *make_pointer_type_no_hash(type_t *type)
{
	pointer_type_t *pointer_type = allocate_type_zero(sizeof(pointer_type[0]));

	pointer_type->type.type = TYPE_POINTER;
	pointer_type->points_to = type;

	return (type_t*) pointer_type;	
}

type_t *parse_type(void)
{
	type_t *type;

	switch(token.type) {
	case T_unsigned:
	case T_signed:
	case T_bool:
	case T_int:
	case T_long:
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
	case '(':
		next_token();
		type = parse_type();
		expect(')');
		break;
	default:
		parser_print_error_prefix();
		fprintf(stderr, "Token ");
		print_token(stderr, &token);
		fprintf(stderr, " doesn't start a type\n");
		type = type_invalid;
		break;
	}

	/* parse type modifiers */
	array_type_t   *array_type;
	while(1) {
		switch(token.type) {
		case '*': {
			next_token();
			type = make_pointer_type_no_hash(type);
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
expression_t *parse_string_const(unsigned precedence)
{
	(void) precedence;

	string_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_STRING_CONST;
	cnst->value           = token.v.string;

	next_token();

	return (expression_t*) cnst;
}

static
expression_t *parse_int_const(unsigned precedence)
{
	(void) precedence;

	int_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_INT_CONST;
	cnst->value           = token.v.intvalue;

	next_token();

	return (expression_t*) cnst;
}

static
expression_t *parse_true(unsigned precedence)
{
	(void) precedence;
	eat(T_true);

	bool_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_BOOL_CONST;
	cnst->value           = 1;

	return (expression_t*) cnst;
}

static
expression_t *parse_false(unsigned precedence)
{
	(void) precedence;
	eat(T_false);

	bool_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_BOOL_CONST;
	cnst->value           = 0;

	return (expression_t*) cnst;
}

static
expression_t *parse_null(unsigned precedence)
{
	(void) precedence;

	eat(T_null);

	null_pointer_t *expression      = allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type     = EXPR_NULL_POINTER;
	expression->expression.datatype = make_pointer_type(type_void);

	return (expression_t*) expression;
}

static
expression_t *parse_func_expression(unsigned precedence)
{
	(void) precedence;

	eat(T_func);

	func_expression_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type   = EXPR_FUNC;

	parse_method(&expression->method);

	return (expression_t*) expression;
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
expression_t *parse_reference(unsigned precedence)
{
	(void) precedence;

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
expression_t *parse_sizeof(unsigned precedence)
{
	(void) precedence;

	eat(T_sizeof);

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

void register_declaration_parser(parse_declaration_function parser,
                                 int token_type)
{
	if(token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(declaration_parsers);
	if(token_type >= len) {
		ARR_RESIZE(declaration_parsers, token_type + 1);
		memset(& declaration_parsers[len], 0,
				(token_type - len + 1) * sizeof(declaration_parsers[0]));
	}

	if(declaration_parsers[token_type] != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple namespace parsers for 1 token");
	}
	declaration_parsers[token_type] = parser;
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
CREATE_UNARY_EXPRESSION_PARSER('~', UNEXPR_BITWISE_NOT);
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
CREATE_BINEXPR_PARSER('%', BINEXPR_MOD);
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
CREATE_BINEXPR_PARSER(T_ANDAND, BINEXPR_LAZY_AND);
CREATE_BINEXPR_PARSER(T_PIPEPIPE, BINEXPR_LAZY_OR);
CREATE_BINEXPR_PARSER(T_LESSLESS, BINEXPR_SHIFTLEFT);
CREATE_BINEXPR_PARSER(T_GREATERGREATER, BINEXPR_SHIFTRIGHT);

static
void register_expression_parsers(void)
{
	register_expression_infix_parser(parse_BINEXPR_MUL,       '*', 16);
	register_expression_infix_parser(parse_BINEXPR_DIV,       '/', 16);
	register_expression_infix_parser(parse_BINEXPR_MOD,       '%', 16);
	register_expression_infix_parser(parse_BINEXPR_SHIFTLEFT, 
	                           T_LESSLESS, 16);
	register_expression_infix_parser(parse_BINEXPR_SHIFTRIGHT,
	                           T_GREATERGREATER, 16);
	register_expression_infix_parser(parse_BINEXPR_ADD,       '+',         15);
	register_expression_infix_parser(parse_BINEXPR_SUB,       '-',         15);
	register_expression_infix_parser(parse_BINEXPR_LESS,      '<',         14);
	register_expression_infix_parser(parse_BINEXPR_GREATER,   '>',         14);
	register_expression_infix_parser(parse_BINEXPR_LESSEQUAL, T_LESSEQUAL, 14);
	register_expression_infix_parser(parse_BINEXPR_GREATEREQUAL,
	                           T_GREATEREQUAL, 14);
	register_expression_infix_parser(parse_BINEXPR_EQUAL,    '=',          13);
	register_expression_infix_parser(parse_BINEXPR_NOTEQUAL, T_SLASHEQUAL, 13);
	register_expression_infix_parser(parse_BINEXPR_AND,      '&',          12);
	register_expression_infix_parser(parse_BINEXPR_LAZY_AND, T_ANDAND,     12);
	register_expression_infix_parser(parse_BINEXPR_XOR,      '^',          11);
	register_expression_infix_parser(parse_BINEXPR_OR,       '|',          10);
	register_expression_infix_parser(parse_BINEXPR_LAZY_OR,  T_PIPEPIPE,   10);
	register_expression_infix_parser(parse_BINEXPR_ASSIGN,   T_ASSIGN,      2);

	register_expression_infix_parser(parse_array_expression,  '[', 25);

	register_expression_infix_parser(parse_call_expression,   '(', 30);
	register_expression_infix_parser(parse_select_expression, '.', 30);

	register_expression_parser(parse_UNEXPR_NEGATE,           '-',    25);
	register_expression_parser(parse_UNEXPR_NOT,              '!',    25);
	register_expression_parser(parse_UNEXPR_BITWISE_NOT,      '~',    25);

	register_expression_parser(parse_UNEXPR_DEREFERENCE,      '*',    20);
	register_expression_parser(parse_UNEXPR_TAKE_ADDRESS,     '&',    20);
	register_expression_parser(parse_cast_expression,         T_cast,  3);

	register_expression_parser(parse_brace_expression,     '(',              1);
	register_expression_parser(parse_sizeof,               T_sizeof,         1);
	register_expression_parser(parse_int_const,            T_INTEGER,        1);
	register_expression_parser(parse_true,                 T_true,           1);
	register_expression_parser(parse_false,                T_false,          1);
	register_expression_parser(parse_string_const,         T_STRING_LITERAL, 1);
	register_expression_parser(parse_null,                 T_null,           1);
	register_expression_parser(parse_reference,            T_IDENTIFIER,     1);
	register_expression_parser(parse_func_expression,      T_func,           1);
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
		left = expected_expression_error();
	}
	assert(left != NULL);
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
		assert(left != NULL);
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
	label->statement.type    = STATEMENT_LABEL;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing label", T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	label->declaration.declaration.type            = DECLARATION_LABEL;
	label->declaration.declaration.source_position = lexer.source_position;
	label->declaration.declaration.symbol          = token.v.symbol;
	next_token();

	add_declaration((declaration_t*) &label->declaration);

	expect(T_NEWLINE);

	return (statement_t*) label;
}

static
statement_t *parse_sub_block(void)
{
	if(token.type != T_NEWLINE) {
		return parse_statement();
	}
	eat(T_NEWLINE);

	if(token.type != T_INDENT) {
		/* create an empty block */
		block_statement_t *block = allocate_ast_zero(sizeof(block[0]));
		block->statement.type = STATEMENT_BLOCK;
		return (statement_t*) block;
	}
		
	return parse_block();
}

static
statement_t *parse_if_statement(void)
{
	eat(T_if);

	expression_t *condition = parse_expression();
	expect(':');

	statement_t *true_statement  = parse_sub_block();
	statement_t *false_statement = NULL;
	if(token.type == T_else) {
		next_token();
		expect(':');
		false_statement = parse_sub_block();
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
	statement_t *first_statement = NULL;
	statement_t *last_statement  = NULL;

	eat(T_var);

	while(1) {
		if(token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing variable declaration",
			                     T_IDENTIFIER, 0);
			eat_until_newline();
			return NULL;
		}

		variable_declaration_statement_t *declaration_statement
			= allocate_ast_zero(sizeof(declaration_statement[0]));
		declaration_statement->statement.type = STATEMENT_VARIABLE_DECLARATION;

		declaration_t *declaration 
			= &declaration_statement->declaration.declaration;
		declaration->type            = DECLARATION_VARIABLE;
		declaration->source_position = lexer.source_position;
		declaration->symbol          = token.v.symbol;
		next_token();

		add_declaration(declaration);

		variable_declaration_t *variable_declaration
			= &declaration_statement->declaration;

		if(token.type == ':') {
			next_token();
			variable_declaration->type = parse_type();
		}

		/* append multiple variable declarations */
		if(last_statement != NULL) {
			last_statement->next = (statement_t*) declaration_statement;
		} else {
			first_statement = (statement_t*) declaration_statement;
		}
		last_statement = (statement_t*) declaration_statement;

		/* do we have an assignment expression? */
		if(token.type == T_ASSIGN) {
			next_token();
			statement_t *assign = parse_initial_assignment(declaration->symbol);

			last_statement->next = assign;
			last_statement       = assign;
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
	statement_t       *statement       = NULL;
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
		parse_declaration_function declaration_parser = NULL;
		if(token.type < ARR_LEN(declaration_parsers))
			declaration_parser = declaration_parsers[token.type];

		if(declaration_parser != NULL) {
			declaration_parser();
		} else {
			statement = parse_expression_statement();
		}
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
	eat(T_INDENT);

	block_statement_t *block = allocate_ast_zero(sizeof(block[0]));
	block->statement.type    = STATEMENT_BLOCK;

	context_t *last_context = current_context;
	current_context         = &block->context;

	statement_t *last_statement = NULL;
	while(token.type != T_DEDENT && token.type != T_EOF) {
		/* parse statement */
		statement_t *statement = parse_statement();
		if(statement == NULL)
			continue;

		if(last_statement != NULL) {
			last_statement->next = statement;
		} else {
			block->statements = statement;
		}
		last_statement = statement;
		/* the parse rule might have produced multiple statements */
		while(last_statement->next != NULL)
			last_statement = last_statement->next;
	}

	assert(current_context == &block->context);
	current_context = last_context;

	expect(T_DEDENT);

	return (statement_t*) block;
}

static
void parse_parameter_declaration(method_type_t *method_type,
                                 method_parameter_t **parameters)
{
	assert(method_type != NULL);

	if(token.type == ')')
		return;

	method_parameter_type_t *last_type = NULL;
	method_parameter_t      *last_param = NULL;
	if(parameters != NULL)
		*parameters = NULL;

	while(1) {
		if(token.type == T_DOTDOTDOT) {
			method_type->variable_arguments = 1;
			next_token();

			if(token.type == ',') {
				parse_error("'...' has to be the last argument in a function "
				            "parameter list");
				eat_until_newline();
				return;
			}
			break;
		}

		if(token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing parameter",
			                     T_IDENTIFIER, 0);
			eat_until_newline();
			return;
		}
		symbol_t *symbol = token.v.symbol;
		next_token();

		expect_void(':');

		method_parameter_type_t *param_type
			= allocate_ast_zero(sizeof(param_type[0]));
		param_type->type = parse_type();

		if(last_type != NULL) {
			last_type->next = param_type;
		} else {
			method_type->parameter_types = param_type;
		}
		last_type = param_type;

		if(parameters != NULL) {
			method_parameter_t *method_param
				= allocate_ast_zero(sizeof(method_param[0]));
			method_param->declaration.type 
				= DECLARATION_METHOD_PARAMETER;
			method_param->declaration.symbol          = symbol;
			method_param->declaration.source_position = lexer.source_position;
			method_param->type                        = param_type->type;

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
type_constraint_t *parse_type_constraints(void)
{
	type_constraint_t *first_constraint = NULL;
	type_constraint_t *last_constraint  = NULL;

	while(token.type == T_IDENTIFIER) {
		type_constraint_t *constraint 
			= allocate_ast_zero(sizeof(constraint[0]));

		constraint->typeclass_symbol = token.v.symbol;
		next_token();

		if(last_constraint == NULL) {
			first_constraint = constraint;
		} else {
			last_constraint->next = constraint;
		}
		last_constraint = constraint;
	}

	return first_constraint;
}

static
type_variable_t *parse_type_parameter(void)
{
	type_variable_t *type_variable
		= allocate_ast_zero(sizeof(type_variable[0]));
	type_variable->declaration.type = DECLARATION_TYPE_VARIABLE;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing type parameter",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	type_variable->declaration.source_position = lexer.source_position;
	type_variable->declaration.symbol          = token.v.symbol;
	next_token();

	if(token.type == ':') {
		next_token();
		type_variable->constraints = parse_type_constraints();
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

void add_declaration(declaration_t *declaration)
{
	assert(declaration != NULL);
	assert(declaration->source_position.input_name != NULL);
	assert(current_context != NULL);

	declaration->next             = current_context->declarations;
	current_context->declarations = declaration;
}

static
void parse_method(method_t *method)
{
	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	method_type->type.type     = TYPE_METHOD;

	context_t *last_context = current_context;
	current_context         = &method->context;

	if(token.type == '<') {
		next_token();
		method->type_parameters = parse_type_parameters();

		/* add type parameters to context */
		type_variable_t *type_parameter = method->type_parameters;
		while(type_parameter != NULL) {
			declaration_t *declaration    = (declaration_t*) type_parameter;
			declaration->next             = current_context->declarations;
			current_context->declarations = declaration;

			type_parameter = type_parameter->next;
		}

		expect_void('>');
	}

	expect_void('(');

	parse_parameter_declaration(method_type, &method->parameters);
	method->type = method_type;

	/* add parameters to context */
	method_parameter_t *parameter = method->parameters;
	while(parameter != NULL) {
		declaration_t *declaration    = (declaration_t*) parameter;
		declaration->next             = current_context->declarations;
		current_context->declarations = declaration;

		parameter = parameter->next;
	}

	expect_void(')');

	method_type->result_type = type_void;
	if(token.type == ':') {
		next_token();
		if(token.type == T_NEWLINE) {
			method->statement = parse_sub_block();
			goto method_parser_end;
		}

		method_type->result_type = parse_type();

		if(token.type == ':') {
			next_token();
			method->statement = parse_sub_block();
			goto method_parser_end;
		}
	}
	expect_void(T_NEWLINE);

method_parser_end:
	assert(current_context == &method->context);
	current_context = last_context;
}

static
void parse_method_declaration(void)
{
	eat(T_func);

	method_declaration_t *method_declaration
		= allocate_ast_zero(sizeof(method_declaration[0]));
	method_declaration->declaration.type            = DECLARATION_METHOD;

	if(token.type == T_extern) {
		method_declaration->method.is_extern = 1;
		next_token();
	}

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing function",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return;
	}
	method_declaration->declaration.source_position = lexer.source_position;
	method_declaration->declaration.symbol          = token.v.symbol;
	next_token();

	parse_method(&method_declaration->method);

	add_declaration((declaration_t*) method_declaration);
}

static
void parse_global_variable(void)
{
	eat(T_var);

	variable_declaration_t *variable = allocate_ast_zero(sizeof(variable[0]));
	variable->declaration.type = DECLARATION_VARIABLE;
	variable->is_global        = 1;

	if(token.type == T_extern) {
		next_token();
		variable->is_extern = 1;
	}

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing global variable",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return;
	}

	variable->declaration.source_position = lexer.source_position;
	variable->declaration.symbol          = token.v.symbol;
	next_token();

	if(token.type != ':') {
		parse_error_expected("global variables must have a type specified",
		                     ':', 0);
		eat_until_newline();
	} else {
		next_token();
		variable->type = parse_type();
		expect_void(T_NEWLINE);
	}

	add_declaration((declaration_t*) variable);
}

static
void parse_constant(void)
{
	eat(T_const);

	constant_t *constant       = allocate_ast_zero(sizeof(constant[0]));
	constant->declaration.type = DECLARATION_CONSTANT;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing constant", T_IDENTIFIER, 0);
		eat_until_newline();
		return;
	}
	constant->declaration.source_position = lexer.source_position; 
	constant->declaration.symbol          = token.v.symbol;
	next_token();

	if(token.type == ':') {
		next_token();
		constant->type = parse_type();
	}

	expect_void(T_ASSIGN);
	constant->expression = parse_expression();

	expect_void(T_NEWLINE);
	add_declaration((declaration_t*) constant);
}

static
void parse_typealias(void)
{
	eat(T_typealias);

	typealias_t *typealias      = allocate_ast_zero(sizeof(typealias[0]));
	typealias->declaration.type = DECLARATION_TYPEALIAS;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typealias",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return;
	}
	typealias->declaration.source_position = lexer.source_position;
	typealias->declaration.symbol          = token.v.symbol;
	next_token();

	expect_void(T_ASSIGN);
	typealias->type = parse_type();

	expect_void(T_NEWLINE);
	add_declaration((declaration_t*) typealias);
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
void parse_struct(void)
{
	eat(T_struct);

	typealias_t *typealias      = allocate_ast_zero(sizeof(typealias[0]));
	typealias->declaration.type = DECLARATION_TYPEALIAS;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing struct",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return;
	}
	typealias->declaration.source_position = lexer.source_position;
	typealias->declaration.symbol          = token.v.symbol;
	next_token();

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_STRUCT;
	compound_type->symbol     = typealias->declaration.symbol;
	compound_type->attributes = parse_attributes();

	typealias->type = (type_t*) compound_type;

	expect_void(':');
	expect_void(T_NEWLINE);

	if(token.type == T_INDENT) {
		compound_type->entries = parse_compound_entries();
	}

	add_declaration((declaration_t*) typealias);
}

static
void parse_union(void)
{
	eat(T_union);

	typealias_t *typealias      = allocate_ast_zero(sizeof(typealias[0]));
	typealias->declaration.type = DECLARATION_TYPEALIAS;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing union",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return;
	}
	typealias->declaration.source_position = lexer.source_position;
	typealias->declaration.symbol          = token.v.symbol;
	next_token();

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_UNION;
	compound_type->symbol     = typealias->declaration.symbol;
	compound_type->attributes = parse_attributes();

	typealias->type = (type_t*) compound_type;

	expect_void(':');
	expect_void(T_NEWLINE);

	if(token.type == T_INDENT) {
		compound_type->entries = parse_compound_entries();
	}

	add_declaration((declaration_t*) typealias);
}

static
typeclass_method_t *parse_typeclass_method(void)
{
	expect(T_func);

	typeclass_method_t *method = allocate_ast_zero(sizeof(method[0]));
	method->declaration.type   = DECLARATION_TYPECLASS_METHOD;

	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;
	
	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typeclass method",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}

	method->declaration.source_position = lexer.source_position;
	method->declaration.symbol          = token.v.symbol;
	next_token();

	expect('(');
	parse_parameter_declaration(method_type, &method->parameters);
	expect(')');

	if(token.type == ':') {
		next_token();
		method_type->result_type = parse_type();
	} else {
		method_type->result_type = type_void;
	}
	expect(T_NEWLINE);

	method->method_type = method_type;

	add_declaration((declaration_t*) method);

	return method;
}

static
void parse_typeclass(void)
{
	eat(T_typeclass);

	typeclass_t *typeclass      = allocate_ast_zero(sizeof(typeclass[0]));
	typeclass->declaration.type = DECLARATION_TYPECLASS;
	
	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typeclass",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return;
	}

	typeclass->declaration.source_position = lexer.source_position;
	typeclass->declaration.symbol          = token.v.symbol;
	next_token();

	if(token.type == '<') {
		next_token();
		typeclass->type_parameters = parse_type_parameters();

		/* add type parameters to context */
		context_t       *context       = &typeclass->context;
		type_variable_t *type_parameter = typeclass->type_parameters;
		while(type_parameter != NULL) {
			declaration_t *declaration = (declaration_t*) type_parameter;
			declaration->next          = context->declarations;
			context->declarations      = declaration;

			type_parameter = type_parameter->next;
		}

		expect_void('>');
	}
	expect_void(':');
	expect_void(T_NEWLINE);

	if(token.type != T_INDENT) {
		goto end_of_parse_typeclass;
	}
	next_token();

	typeclass_method_t *last_method = NULL;
	while(token.type != T_DEDENT) {
		if(token.type == T_EOF) {
			parse_error("EOF while parsing typeclass");
			goto end_of_parse_typeclass;
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

end_of_parse_typeclass:
	add_declaration((declaration_t*) typeclass);
}

static
typeclass_method_instance_t *parse_typeclass_method_instance(void)
{
	typeclass_method_instance_t *method_instance
		= allocate_ast_zero(sizeof(method_instance[0]));

	expect(T_func);
	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typeclass method "
		                     "instance", T_IDENTIFIER, 0);
		eat_until_newline();
		return NULL;
	}
	method_instance->source_position = lexer.source_position;
	method_instance->symbol          = token.v.symbol;
	next_token();

	parse_method(& method_instance->method);

	return method_instance;
}

static
void parse_typeclass_instance(void)
{
	eat(T_instance);

	typeclass_instance_t *instance = allocate_ast_zero(sizeof(instance[0]));
	instance->source_position      = lexer.source_position;

	if(token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typeclass instance",
		                     T_IDENTIFIER, 0);
		eat_until_newline();
		return;
	}

	instance->typeclass_symbol = token.v.symbol;
	next_token();

	instance->type_arguments = parse_type_arguments();
	
	expect_void(':');
	expect_void(T_NEWLINE);

	if(token.type != T_INDENT) {
		goto add_instance;
	}
	eat(T_INDENT);

	typeclass_method_instance_t *last_method = NULL;
	while(token.type != T_DEDENT) {
		if(token.type == T_EOF) {
			parse_error("EOF while parsing typeclass instance");
			return;
		}

		typeclass_method_instance_t *method	= parse_typeclass_method_instance();
		if(method == NULL)
			continue;

		if(last_method != NULL) {
			last_method->next = method;
		} else {
			instance->method_instances = method;
		}
		last_method = method;
	}
	eat(T_DEDENT);

add_instance:
	assert(current_context != NULL);
	instance->next                       = current_context->typeclass_instances;
	current_context->typeclass_instances = instance;
}

void parse_declaration(void)
{
	if(token.type < 0) {
		if(token.type == T_EOF)
			return;

		/* this shouldn't happen if the lexer is correct... */
		parse_error_expected("problem while parsing namespace entry",
		                     T_DEDENT, 0);
		return;
	}

	parse_declaration_function parser = NULL;
	if(token.type < ARR_LEN(declaration_parsers))
		parser = declaration_parsers[token.type];

	if(parser == NULL) {
		parse_error_expected("Couldn't parse compilation unit entry",
		                     T_func, T_var, T_extern, T_struct, T_typeclass,
		                     T_instance, 0);
		eat_until_newline();
		return;
	}

	if(parser != NULL) {
		parser();
	}
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

	namespace         = allocate_ast_zero(sizeof(namespace[0]));
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
	assert(current_context == NULL);
	current_context        = &namespace->context;

	/* parse namespace entries */
	while(token.type != T_EOF) {
		parse_declaration();
	}

	assert(current_context == &namespace->context);
	current_context = NULL;

	return namespace;
}

static
void skip_declaration(void)
{
	next_token();
}

static
void register_declaration_parsers(void)
{
	register_declaration_parser(parse_method_declaration, T_func);
	register_declaration_parser(parse_global_variable,    T_var);
	register_declaration_parser(parse_constant,           T_const);
	register_declaration_parser(parse_struct,             T_struct);
	register_declaration_parser(parse_union,              T_union);
	register_declaration_parser(parse_typealias,          T_typealias);
	register_declaration_parser(parse_typeclass,          T_typeclass);
	register_declaration_parser(parse_typeclass_instance, T_instance);
	register_declaration_parser(skip_declaration,         T_NEWLINE);
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
	expression_parsers  = NEW_ARR_F(expression_parse_function_t, 0);
	statement_parsers   = NEW_ARR_F(parse_statement_function, 0);
	declaration_parsers = NEW_ARR_F(parse_declaration_function, 0);
	attribute_parsers   = NEW_ARR_F(parse_attribute_function, 0);

	register_expression_parsers();
	register_statement_parsers();
	register_declaration_parsers();
}

void exit_parser(void)
{
	DEL_ARR_F(attribute_parsers);
	DEL_ARR_F(declaration_parsers);
	DEL_ARR_F(expression_parsers);
	DEL_ARR_F(statement_parsers);
}

