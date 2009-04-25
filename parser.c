#include <config.h>

#include "parser_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdarg.h>

#include "symbol_table_t.h"
#include "lexer.h"
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

static expression_parse_function_t *expression_parsers  = NULL;
static parse_statement_function    *statement_parsers   = NULL;
static parse_declaration_function  *declaration_parsers = NULL;
static parse_attribute_function    *attribute_parsers   = NULL;

static unsigned char token_anchor_set[T_LAST_TOKEN];

static context_t *current_context = NULL;

static int      error = 0;
       token_t  token;

static inline void *allocate_ast_zero(size_t size)
{
	void *res = allocate_ast(size);
	memset(res, 0, size);
	return res;
}

static inline void *allocate_type_zero(size_t size)
{
	void *res = obstack_alloc(type_obst, size);
	memset(res, 0, size);
	return res;
}

void next_token(void)
{
	lexer_next_token(&token);

#ifdef PRINT_TOKENS
	print_token(stderr, &token);
	fprintf(stderr, "\n");
#endif
}

static void replace_token_type(token_type_t type)
{
	token.type = type;
}

static inline void eat(token_type_t type)
{
	assert(token.type == type);
	next_token();
}

static void add_anchor_token(int token_type)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	++token_anchor_set[token_type];
}

static void rem_anchor_token(int token_type)
{
	assert(0 <= token_type && token_type < T_LAST_TOKEN);
	assert(token_anchor_set[token_type] != 0);
	--token_anchor_set[token_type];
}

static inline void parser_found_error(void)
{
	error = 1;
#ifdef ABORT_ON_ERROR
	abort();
#endif
}

void parser_print_error_prefix(void)
{
	fputs(source_position.input_name, stderr);
	fputc(':', stderr);
	fprintf(stderr, "%d", source_position.linenr);
	fputs(": error: ", stderr);
	parser_found_error();
}

static void parse_error(const char *message)
{
	parser_print_error_prefix();
	fprintf(stderr, "parse error: %s\n", message);
}

static void parse_error_expected(const char *message, ...)
{
	va_list args;
	int first = 1;

	if (message != NULL) {
		parser_print_error_prefix();
		fprintf(stderr, "%s\n", message);
	}
	parser_print_error_prefix();
	fputs("Parse error: got ", stderr);
	print_token(stderr, &token);
	fputs(", expected ", stderr);

	va_start(args, message);
	token_type_t token_type = va_arg(args, token_type_t);
	while (token_type != 0) {
		if (first == 1) {
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
static void maybe_eat_block(void)
{
	if (token.type != T_INDENT)
		return;
	next_token();

	unsigned indent = 1;
	while (indent >= 1) {
		if (token.type == T_INDENT) {
			indent++;
		} else if (token.type == T_DEDENT) {
			indent--;
		} else if (token.type == T_EOF) {
			break;
		}
		next_token();
	}
}

/**
 * eats nested brace groups
 */
static void eat_until_matching_token(int type)
{
	int end_token;
	switch (type) {
		case '(': end_token = ')';  break;
		case '{': end_token = '}';  break;
		case '[': end_token = ']';  break;
		default:  end_token = type; break;
	}

	unsigned parenthesis_count = 0;
	unsigned brace_count       = 0;
	unsigned bracket_count     = 0;
	while (token.type        != end_token ||
	       parenthesis_count != 0         ||
	       brace_count       != 0         ||
	       bracket_count     != 0) {
		switch (token.type) {
		case T_EOF: return;
		case '(': ++parenthesis_count; break;
		case '{': ++brace_count;       break;
		case '[': ++bracket_count;     break;

		case ')':
			if (parenthesis_count > 0)
				--parenthesis_count;
			goto check_stop;

		case '}':
			if (brace_count > 0)
				--brace_count;
			goto check_stop;

		case ']':
			if (bracket_count > 0)
				--bracket_count;
check_stop:
			if (token.type        == end_token &&
			    parenthesis_count == 0         &&
			    brace_count       == 0         &&
			    bracket_count     == 0)
				return;
			break;

		default:
			break;
		}
		next_token();
	}
}

/**
 * Eat input tokens until an anchor is found.
 */
static void eat_until_anchor(void)
{
	while (token_anchor_set[token.type] == 0) {
		if (token.type == '(' || token.type == '{' || token.type == '[')
			eat_until_matching_token(token.type);
		if (token.type == ':') {
			next_token();
			if (!token_anchor_set[token.type] == 0) {
				maybe_eat_block();
			}
		} else {
			next_token();
		}
	}
}

#define expect(expected, error_label)                      \
	do {                                                   \
		if (UNLIKELY(token.type != (expected))) {           \
			parse_error_expected(NULL, (expected), 0);     \
			add_anchor_token(expected);                    \
			eat_until_anchor();                            \
			if (token.type == expected)                    \
				next_token();                              \
			rem_anchor_token(expected);                    \
			goto error_label;                              \
		}                                                  \
		next_token();                                      \
	} while (0)                      

static void parse_method(method_t *method);

static statement_t *parse_block(void);

static void parse_parameter_declaration(method_type_t *method_type,
                                 method_parameter_t **parameters);

static atomic_type_type_t parse_unsigned_atomic_type(void)
{
	switch (token.type) {
	case T_byte:
		next_token();
		return ATOMIC_TYPE_UBYTE;
	case T_short:
		next_token();
		return ATOMIC_TYPE_USHORT;
	case T_long:
		next_token();
		if (token.type == T_long) {
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

static atomic_type_type_t parse_signed_atomic_type(void)
{
	switch (token.type) {
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
		if (token.type == T_long) {
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

static type_t *parse_atomic_type(void)
{
	atomic_type_type_t atype;

	switch (token.type) {
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
	if (result != (type_t*) type) {
		obstack_free(type_obst, type);
	}

	return result;
}

static type_argument_t *parse_type_argument(void)
{
	type_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));

	argument->type = parse_type();
	return argument;
}

static type_argument_t *parse_type_arguments(void)
{
	type_argument_t *first_argument = parse_type_argument();
	type_argument_t *last_argument  = first_argument;

	while (token.type == ',') {
		next_token();
		type_argument_t *type_argument = parse_type_argument();

		last_argument->next = type_argument;
		last_argument       = type_argument;
	}

	return first_argument;
}

static type_t *parse_typeof(void)
{
	typeof_type_t *typeof_type = allocate_type_zero(sizeof(typeof_type[0]));
	typeof_type->type.type     = TYPE_TYPEOF;

	eat(T_typeof);
	expect('(', end_error);
	add_anchor_token(')');
	typeof_type->expression = parse_expression();
	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	return (type_t*) typeof_type;
}

static type_t *parse_type_ref(void)
{
	assert(token.type == T_IDENTIFIER);

	type_reference_t *type_ref = allocate_type_zero(sizeof(type_ref[0]));

	type_ref->type.type       = TYPE_REFERENCE;
	type_ref->symbol          = token.v.symbol;
	type_ref->source_position = source_position;
	next_token();

	if (token.type == '<') {
		next_token();
		add_anchor_token('>');
		type_ref->type_arguments = parse_type_arguments();
		rem_anchor_token('>');
		expect('>', end_error);
	}

end_error:
	return (type_t*) type_ref;
}

static type_t *create_error_type(void)
{
	type_t *error_type = allocate_type_zero(sizeof(error_type[0]));
	error_type->type   = TYPE_ERROR;
	return error_type;
}

static type_t *parse_method_type(void)
{
	eat(T_func);

	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;

	expect('(', end_error);
	add_anchor_token(')');
	parse_parameter_declaration(method_type, NULL);
	rem_anchor_token(')');
	expect(')', end_error);
	expect(':', end_error);
	method_type->result_type = parse_type();

	return (type_t*) method_type;

end_error:
	return create_error_type();
}

static compound_entry_t *parse_compound_entries(void)
{
	compound_entry_t *result     = NULL;
	compound_entry_t *last_entry = NULL;
	while (token.type != T_DEDENT && token.type != T_EOF) {
		compound_entry_t *entry = allocate_ast_zero(sizeof(entry[0]));

		if (token.type != T_IDENTIFIER) {
			parse_error_expected("Problem while parsing compound entry",
								 T_IDENTIFIER, 0);
			continue;
		}
		entry->symbol = token.v.symbol;
		next_token();

		expect(':', end_error);
		entry->type       = parse_type();
		entry->attributes = parse_attributes();

		if (last_entry == NULL) {
			result = entry;
		} else {
			last_entry->next = entry;
		}
		last_entry = entry;

		expect(T_NEWLINE, end_error);
	}

end_error:
	return result;
}

static type_t *parse_union_type(void)
{
	eat(T_union);

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_UNION;
	compound_type->attributes = parse_attributes();

	expect(':', end_error);
	expect(T_NEWLINE, end_error);
	expect(T_INDENT, end_error);
	
	add_anchor_token(T_DEDENT);	
	compound_type->entries = parse_compound_entries();

	/* force end of statement */
	rem_anchor_token(T_DEDENT);
	assert(token.type == T_DEDENT);
	replace_token_type(T_NEWLINE);

end_error:
	return (type_t*) compound_type;
}

static type_t *parse_struct_type(void)
{
	eat(T_struct);

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_STRUCT;
	compound_type->attributes = parse_attributes();

	expect(':', end_error);
	expect(T_NEWLINE, end_error);
	expect(T_INDENT, end_error);
	
	add_anchor_token(T_DEDENT);	
	compound_type->entries = parse_compound_entries();

	/* force end of statement */
	rem_anchor_token(T_DEDENT);
	assert(token.type == T_DEDENT);
	replace_token_type(T_NEWLINE);

end_error:
	return (type_t*) compound_type;
}

static type_t *make_pointer_type_no_hash(type_t *type)
{
	pointer_type_t *pointer_type = allocate_type_zero(sizeof(pointer_type[0]));

	pointer_type->type.type = TYPE_POINTER;
	pointer_type->points_to = type;

	return (type_t*) pointer_type;	
}

static type_t *parse_brace_type(void)
{
	eat('(');
	add_anchor_token(')');
	type_t *type = parse_type();
	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	return type;
}

type_t *parse_type(void)
{
	type_t *type;

	switch (token.type) {
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
	case T_typeof:
		type = parse_typeof();
		break;
	case T_void:
		type = type_void;
		next_token();
		break;
	case T_union:
		type = parse_union_type();
		break;
	case T_struct:
		type = parse_struct_type();
		break;
	case T_func:
		type = parse_method_type();
		break;
	case '(':
		type = parse_brace_type();
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
	while (true) {
		switch (token.type) {
		case '*': {
			next_token();
			type = make_pointer_type_no_hash(type);
			break;
		}
		case '[': {
			next_token();
			add_anchor_token(']');
			if (token.type != T_INTEGER) {
				parse_error_expected("problem while parsing array type",
				                     T_INTEGER, 0);
				eat_until_anchor();
				rem_anchor_token(']');
				break;
			}
			int size = token.v.intvalue;
			next_token();

			if (size < 0) {
				parse_error("negative array size not allowed");
				eat_until_anchor();
				rem_anchor_token(']');
				break;
			}

			array_type_t *array_type = allocate_type_zero(sizeof(array_type[0]));

			array_type->type.type    = TYPE_ARRAY;
			array_type->element_type = type;
			array_type->size         = size;

			type = (type_t*) array_type;

			rem_anchor_token(']');
			expect(']', end_error);
			break;
			}
		default:
			return type;
		}
	}

end_error:
	return type;
}


static expression_t *parse_string_const(void)
{
	string_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_STRING_CONST;
	cnst->value           = token.v.string;

	next_token();

	return (expression_t*) cnst;
}

static expression_t *parse_int_const(void)
{
	int_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_INT_CONST;
	cnst->value           = token.v.intvalue;

	next_token();

	return (expression_t*) cnst;
}

static expression_t *parse_true(void)
{
	eat(T_true);

	bool_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_BOOL_CONST;
	cnst->value           = 1;

	return (expression_t*) cnst;
}

static expression_t *parse_false(void)
{
	eat(T_false);

	bool_const_t *cnst = allocate_ast_zero(sizeof(cnst[0]));

	cnst->expression.type = EXPR_BOOL_CONST;
	cnst->value           = 0;

	return (expression_t*) cnst;
}

static expression_t *parse_null(void)
{
	eat(T_null);

	null_pointer_t *expression      = allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type     = EXPR_NULL_POINTER;
	expression->expression.datatype = make_pointer_type(type_void);

	return (expression_t*) expression;
}

static expression_t *parse_func_expression(void)
{
	eat(T_func);

	func_expression_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type   = EXPR_FUNC;

	parse_method(&expression->method);

	return (expression_t*) expression;
}

static expression_t *parse_reference(void)
{
	reference_expression_t *ref = allocate_ast_zero(sizeof(ref[0]));

	ref->expression.type = EXPR_REFERENCE;
	ref->symbol          = token.v.symbol;

	next_token();

	if (token.type == T_TYPESTART) {
		next_token();
		add_anchor_token('>');
		ref->type_arguments = parse_type_arguments();
		rem_anchor_token('>');
		expect('>', end_error);
	}

end_error:
	return (expression_t*) ref;
}

static expression_t *create_error_expression(void)
{
	expression_t *expression = allocate_ast_zero(sizeof(expression[0]));
	expression->type         = EXPR_ERROR;
	expression->datatype     = create_error_type();
	return expression;
}

static expression_t *parse_sizeof(void)
{
	eat(T_sizeof);

	sizeof_expression_t *expression	= allocate_ast_zero(sizeof(expression[0]));
	expression->expression.type = EXPR_SIZEOF;

	if (token.type == '(') {
		next_token();
		typeof_type_t *typeof_type = allocate_type_zero(sizeof(typeof_type[0]));
		typeof_type->type.type     = TYPE_TYPEOF;
		expression->type           = (type_t*) typeof_type;

		add_anchor_token(')');
		typeof_type->expression = parse_expression();
		rem_anchor_token(')');
		expect(')', end_error);
	} else {
		expect('<', end_error);
		add_anchor_token('>');
		expression->type = parse_type();
		rem_anchor_token('>');
		expect('>', end_error);
	}
	return (expression_t*) expression;

end_error:
	return create_error_expression();
}

void register_statement_parser(parse_statement_function parser, int token_type)
{
	if (token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(statement_parsers);
	if (token_type >= len) {
		ARR_RESIZE(parse_statement_function, statement_parsers, token_type + 1);
		memset(& statement_parsers[len], 0,
				(token_type - len + 1) * sizeof(statement_parsers[0]));
	}

	if (statement_parsers[token_type] != NULL) {
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
	if (token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(declaration_parsers);
	if (token_type >= len) {
		ARR_RESIZE(parse_declaration_function, declaration_parsers, token_type + 1);
		memset(& declaration_parsers[len], 0,
				(token_type - len + 1) * sizeof(declaration_parsers[0]));
	}

	if (declaration_parsers[token_type] != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple namespace parsers for 1 token");
	}
	declaration_parsers[token_type] = parser;
}

void register_attribute_parser(parse_attribute_function parser, int token_type)
{
	if (token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(attribute_parsers);
	if (token_type >= len) {
		ARR_RESIZE(parse_attribute_function, attribute_parsers, token_type + 1);
		memset(& attribute_parsers[len], 0,
				(token_type - len + 1) * sizeof(attribute_parsers[0]));
	}

	if (attribute_parsers[token_type] != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple namespace parsers for 1 token");
	}
	attribute_parsers[token_type] = parser;
}

static expression_parse_function_t *get_expression_parser_entry(int token_type)
{
	if (token_type < 0)
		panic("can't register parser for negative token");

	int len = ARR_LEN(expression_parsers);
	if (token_type >= len) {
		ARR_RESIZE(expression_parse_function_t, expression_parsers, token_type + 1);
		memset(& expression_parsers[len], 0,
				(token_type - len + 1) * sizeof(expression_parsers[0]));
	}

	return &expression_parsers[token_type];
}

void register_expression_parser(parse_expression_function parser,
                                int token_type)
{
	expression_parse_function_t *entry
		= get_expression_parser_entry(token_type);

	if (entry->parser != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple expression parsers for a token");
	}
	entry->parser     = parser;
}

void register_expression_infix_parser(parse_expression_infix_function parser,
                                      int token_type, unsigned precedence)
{
	expression_parse_function_t *entry
		= get_expression_parser_entry(token_type);

	if (entry->infix_parser != NULL) {
		fprintf(stderr, "for token ");
		print_token_type(stderr, token_type);
		fprintf(stderr, "\n");
		panic("trying to register multiple infix expression parsers for a "
		      "token");
	}
	entry->infix_parser     = parser;
	entry->infix_precedence = precedence;
}

static expression_t *expected_expression_error(void)
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

static expression_t *parse_parenthesized_expression(void)
{
	eat('(');

	add_anchor_token(')');
	expression_t *result = parse_expression();
	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	return result;
}

static expression_t *parse_cast_expression(void)
{
	eat(T_cast);

	unary_expression_t *unary_expression 
		= allocate_ast_zero(sizeof(unary_expression[0]));
	unary_expression->expression.type            = EXPR_UNARY;
	unary_expression->type                       = UNEXPR_CAST;
	
	expect('<', end_error);
	unary_expression->expression.datatype = parse_type();
	expect('>', end_error);

	unary_expression->value = parse_sub_expression(PREC_CAST);

end_error:
	return (expression_t*) unary_expression;
}

static expression_t *parse_call_expression(expression_t *expression)
{
	call_expression_t *call = allocate_ast_zero(sizeof(call[0]));

	call->expression.type = EXPR_CALL;
	call->method          = expression;

	/* parse arguments */
	eat('(');

	add_anchor_token(')');
	add_anchor_token(',');

	if (token.type != ')') {
		call_argument_t *last_argument = NULL;

		while (true) {
			call_argument_t *argument = allocate_ast_zero(sizeof(argument[0]));

			argument->expression = parse_expression();
			if (last_argument == NULL) {
				call->arguments = argument;
			} else {
				last_argument->next = argument;
			}
			last_argument = argument;

			if (token.type != ',')
				break;
			next_token();
		}
	}
	rem_anchor_token(',');
	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	return (expression_t*) call;
}

static expression_t *parse_select_expression(expression_t *compound)
{
	eat('.');

	select_expression_t *select = allocate_ast_zero(sizeof(select[0]));

	select->expression.type            = EXPR_SELECT;
	select->compound                   = compound;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing compound select",
		                     T_IDENTIFIER, 0);
		return NULL;
	}
	select->symbol          = token.v.symbol;
	next_token();

	return (expression_t*) select;
}

static expression_t *parse_array_expression(expression_t *array_ref)
{
	eat('[');

	array_access_expression_t *array_access
		= allocate_ast_zero(sizeof(array_access[0]));

	array_access->expression.type = EXPR_ARRAY_ACCESS;
	array_access->array_ref       = array_ref;
	array_access->index           = parse_expression();

	if (token.type != ']') {
		parse_error_expected("Problem while parsing array access", ']', 0);
		return NULL;
	}
	next_token();

	return (expression_t*) array_access;
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_type, unexpression_type)     \
static expression_t *parse_##unexpression_type(void)                      \
{                                                                         \
	eat(token_type);                                                      \
                                                                          \
	unary_expression_t *unary_expression                                  \
		= allocate_ast_zero(sizeof(unary_expression[0]));                 \
	unary_expression->expression.type = EXPR_UNARY;                       \
	unary_expression->type            = unexpression_type;                \
	unary_expression->value           = parse_sub_expression(PREC_UNARY); \
                                                                          \
	return (expression_t*) unary_expression;                              \
}

CREATE_UNARY_EXPRESSION_PARSER('-',          UNEXPR_NEGATE)
CREATE_UNARY_EXPRESSION_PARSER('!',          UNEXPR_NOT)
CREATE_UNARY_EXPRESSION_PARSER('~',          UNEXPR_BITWISE_NOT)
CREATE_UNARY_EXPRESSION_PARSER('*',          UNEXPR_DEREFERENCE)
CREATE_UNARY_EXPRESSION_PARSER('&',          UNEXPR_TAKE_ADDRESS)
CREATE_UNARY_EXPRESSION_PARSER(T_PLUSPLUS,   UNEXPR_INCREMENT)
CREATE_UNARY_EXPRESSION_PARSER(T_MINUSMINUS, UNEXPR_DECREMENT)

#define CREATE_BINEXPR_PARSER_RL(token_type, binexpression_type, prec_r)  \
static expression_t *parse_##binexpression_type(expression_t *left)       \
{                                                                         \
	eat(token_type);                                                      \
                                                                          \
	expression_t *right = parse_sub_expression(prec_r);                   \
                                                                          \
	binary_expression_t *binexpr                                          \
		= allocate_ast_zero(sizeof(binexpr[0]));                          \
	binexpr->expression.type            = EXPR_BINARY;                    \
	binexpr->type                       = binexpression_type;             \
	binexpr->left                       = left;                           \
	binexpr->right                      = right;                          \
                                                                          \
	return (expression_t*) binexpr;                                       \
}

#define CREATE_BINEXPR_PARSER_LR(token_type, binexpression_type, prec_r)  \
	CREATE_BINEXPR_PARSER_RL(token_type, binexpression_type, prec_r+1)

CREATE_BINEXPR_PARSER_LR('*', BINEXPR_MUL, PREC_MULTIPLICATIVE);
CREATE_BINEXPR_PARSER_LR('/', BINEXPR_DIV, PREC_MULTIPLICATIVE);
CREATE_BINEXPR_PARSER_LR('%', BINEXPR_MOD, PREC_MULTIPLICATIVE);
CREATE_BINEXPR_PARSER_LR('+', BINEXPR_ADD, PREC_ADDITIVE);
CREATE_BINEXPR_PARSER_LR('-', BINEXPR_SUB, PREC_ADDITIVE);
CREATE_BINEXPR_PARSER_LR('<', BINEXPR_LESS, PREC_RELATIONAL);
CREATE_BINEXPR_PARSER_LR('>', BINEXPR_GREATER, PREC_RELATIONAL);
CREATE_BINEXPR_PARSER_LR(T_EQUALEQUAL, BINEXPR_EQUAL, PREC_EQUALITY);
CREATE_BINEXPR_PARSER_RL('=', BINEXPR_ASSIGN, PREC_ASSIGNMENT);
CREATE_BINEXPR_PARSER_LR(T_SLASHEQUAL, BINEXPR_NOTEQUAL, PREC_EQUALITY);
CREATE_BINEXPR_PARSER_LR(T_LESSEQUAL, BINEXPR_LESSEQUAL, PREC_RELATIONAL);
CREATE_BINEXPR_PARSER_LR(T_GREATEREQUAL, BINEXPR_GREATEREQUAL, PREC_RELATIONAL);
CREATE_BINEXPR_PARSER_LR('&', BINEXPR_AND, PREC_AND);
CREATE_BINEXPR_PARSER_LR('|', BINEXPR_OR, PREC_OR);
CREATE_BINEXPR_PARSER_LR('^', BINEXPR_XOR, PREC_XOR);
CREATE_BINEXPR_PARSER_LR(T_ANDAND, BINEXPR_LAZY_AND, PREC_LAZY_AND);
CREATE_BINEXPR_PARSER_LR(T_PIPEPIPE, BINEXPR_LAZY_OR, PREC_LAZY_OR);
CREATE_BINEXPR_PARSER_LR(T_LESSLESS, BINEXPR_SHIFTLEFT, PREC_MULTIPLICATIVE);
CREATE_BINEXPR_PARSER_LR(T_GREATERGREATER, BINEXPR_SHIFTRIGHT, PREC_MULTIPLICATIVE);

static void register_expression_parsers(void)
{
	register_expression_infix_parser(parse_BINEXPR_MUL,       '*', PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_BINEXPR_DIV,       '/', PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_BINEXPR_MOD,       '%', PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_BINEXPR_SHIFTLEFT, 
	                           T_LESSLESS, PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_BINEXPR_SHIFTRIGHT,
	                           T_GREATERGREATER, PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_BINEXPR_ADD,       '+',         PREC_ADDITIVE);
	register_expression_infix_parser(parse_BINEXPR_SUB,       '-',         PREC_ADDITIVE);
	register_expression_infix_parser(parse_BINEXPR_LESS,      '<',         PREC_RELATIONAL);
	register_expression_infix_parser(parse_BINEXPR_GREATER,   '>',         PREC_RELATIONAL);
	register_expression_infix_parser(parse_BINEXPR_LESSEQUAL, T_LESSEQUAL, PREC_RELATIONAL);
	register_expression_infix_parser(parse_BINEXPR_GREATEREQUAL,
	                           T_GREATEREQUAL, PREC_RELATIONAL);
	register_expression_infix_parser(parse_BINEXPR_EQUAL,    T_EQUALEQUAL, PREC_EQUALITY);
	register_expression_infix_parser(parse_BINEXPR_NOTEQUAL, T_SLASHEQUAL, PREC_EQUALITY);
	register_expression_infix_parser(parse_BINEXPR_AND,      '&',          PREC_AND);
	register_expression_infix_parser(parse_BINEXPR_LAZY_AND, T_ANDAND,     PREC_LAZY_AND);
	register_expression_infix_parser(parse_BINEXPR_XOR,      '^',          PREC_XOR);
	register_expression_infix_parser(parse_BINEXPR_OR,       '|',          PREC_OR);
	register_expression_infix_parser(parse_BINEXPR_LAZY_OR,  T_PIPEPIPE,   PREC_LAZY_OR);
	register_expression_infix_parser(parse_BINEXPR_ASSIGN,   '=',          PREC_ASSIGNMENT);

	register_expression_infix_parser(parse_array_expression,  '[', PREC_POSTFIX);

	register_expression_infix_parser(parse_call_expression,   '(', PREC_POSTFIX);
	register_expression_infix_parser(parse_select_expression, '.', PREC_POSTFIX);

	register_expression_parser(parse_UNEXPR_NEGATE,           '-');
	register_expression_parser(parse_UNEXPR_NOT,              '!');
	register_expression_parser(parse_UNEXPR_BITWISE_NOT,      '~');
	register_expression_parser(parse_UNEXPR_INCREMENT,        T_PLUSPLUS);
	register_expression_parser(parse_UNEXPR_DECREMENT,        T_MINUSMINUS);

	register_expression_parser(parse_UNEXPR_DEREFERENCE,      '*');
	register_expression_parser(parse_UNEXPR_TAKE_ADDRESS,     '&');
	register_expression_parser(parse_cast_expression,         T_cast);

	register_expression_parser(parse_parenthesized_expression,'(');
	register_expression_parser(parse_sizeof,               T_sizeof);
	register_expression_parser(parse_int_const,            T_INTEGER);
	register_expression_parser(parse_true,                 T_true);
	register_expression_parser(parse_false,                T_false);
	register_expression_parser(parse_string_const,         T_STRING_LITERAL);
	register_expression_parser(parse_null,                 T_null);
	register_expression_parser(parse_reference,            T_IDENTIFIER);
	register_expression_parser(parse_func_expression,      T_func);
}

expression_t *parse_sub_expression(unsigned precedence)
{
	if (token.type < 0) {
		return expected_expression_error();
	}

	expression_parse_function_t *parser	
		= & expression_parsers[token.type];
	source_position_t  start = source_position;
	expression_t      *left;
	
	if (parser->parser != NULL) {
		left = parser->parser();
	} else {
		left = expected_expression_error();
	}
	assert(left != NULL);
	left->source_position = start;

	while (true) {
		if (token.type < 0) {
			return expected_expression_error();
		}

		parser = &expression_parsers[token.type];
		if (parser->infix_parser == NULL)
			break;
		if (parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(left);
		assert(left != NULL);
		left->source_position = start;
	}

	return left;
}

expression_t *parse_expression(void)
{
	return parse_sub_expression(1);
}





static statement_t *parse_return_statement(void)
{
	return_statement_t *return_statement =
		allocate_ast_zero(sizeof(return_statement[0]));

	return_statement->statement.type = STATEMENT_RETURN;
	next_token();

	if (token.type != T_NEWLINE) {
		return_statement->return_value = parse_expression();
	}
	expect(T_NEWLINE, end_error);

end_error:
	return (statement_t*) return_statement;
}

static statement_t *create_error_statement(void)
{
	statement_t *statement = allocate_ast_zero(sizeof(statement[0]));
	statement->type = STATEMENT_ERROR;
	return statement;
}

static statement_t *parse_goto_statement(void)
{
	eat(T_goto);

	goto_statement_t *goto_statement
		= allocate_ast_zero(sizeof(goto_statement[0]));
	goto_statement->statement.type = STATEMENT_GOTO;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing goto statement",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		goto end_error;
	}
	goto_statement->label_symbol = token.v.symbol;
	next_token();

	expect(T_NEWLINE, end_error);

	return (statement_t*) goto_statement;

end_error:
	return create_error_statement();
}

static statement_t *parse_label_statement(void)
{
	eat(':');

	label_statement_t *label = allocate_ast_zero(sizeof(label[0]));
	label->statement.type    = STATEMENT_LABEL;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing label", T_IDENTIFIER, 0);
		eat_until_anchor();
		goto end_error;
	}
	label->declaration.declaration.type            = DECLARATION_LABEL;
	label->declaration.declaration.source_position = source_position;
	label->declaration.declaration.symbol          = token.v.symbol;
	next_token();

	add_declaration((declaration_t*) &label->declaration);

	expect(T_NEWLINE, end_error);

	return (statement_t*) label;

end_error:
	return create_error_statement();
}

static statement_t *parse_sub_block(void)
{
	if (token.type != T_NEWLINE) {
		return parse_statement();
	}
	eat(T_NEWLINE);

	if (token.type != T_INDENT) {
		/* create an empty block */
		block_statement_t *block = allocate_ast_zero(sizeof(block[0]));
		block->statement.type = STATEMENT_BLOCK;
		return (statement_t*) block;
	}
		
	return parse_block();
}

static statement_t *parse_if_statement(void)
{
	eat(T_if);

	expression_t *condition = parse_expression();
	expect(':', end_error);

	statement_t *true_statement  = parse_sub_block();
	statement_t *false_statement = NULL;
	if (token.type == T_else) {
		next_token();
		if (token.type == ':')
			next_token();
		false_statement = parse_sub_block();
	}

	if_statement_t *if_statement
		= allocate_ast_zero(sizeof(if_statement[0]));

	if_statement->statement.type  = STATEMENT_IF;
	if_statement->condition       = condition;
	if_statement->true_statement  = true_statement;
	if_statement->false_statement = false_statement;

	return (statement_t*) if_statement;

end_error:
	return create_error_statement();
}

static statement_t *parse_initial_assignment(symbol_t *symbol)
{
	reference_expression_t *ref = allocate_ast_zero(sizeof(ref[0]));
	ref->expression.type = EXPR_REFERENCE;
	ref->symbol          = symbol;

	binary_expression_t *assign = allocate_ast_zero(sizeof(assign[0]));

	assign->expression.type            = EXPR_BINARY;
	assign->expression.source_position = source_position;
	assign->type                       = BINEXPR_ASSIGN;
	assign->left                       = (expression_t*) ref;
	assign->right                      = parse_expression();

	expression_statement_t *expr_statement
		= allocate_ast_zero(sizeof(expr_statement[0]));

	expr_statement->statement.type = STATEMENT_EXPRESSION;
	expr_statement->expression     = (expression_t*) assign;

	return (statement_t*) expr_statement;
}

static statement_t *parse_variable_declaration(void)
{
	statement_t *first_statement = NULL;
	statement_t *last_statement  = NULL;

	eat(T_var);

	while (true) {
		if (token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing variable declaration",
			                     T_IDENTIFIER, 0);
			eat_until_anchor();
			goto end_error;
		}

		variable_declaration_statement_t *declaration_statement
			= allocate_ast_zero(sizeof(declaration_statement[0]));
		declaration_statement->statement.type = STATEMENT_VARIABLE_DECLARATION;

		declaration_t *declaration 
			= &declaration_statement->declaration.declaration;
		declaration->type            = DECLARATION_VARIABLE;
		declaration->source_position = source_position;
		declaration->symbol          = token.v.symbol;
		next_token();

		add_declaration(declaration);

		variable_declaration_t *variable_declaration
			= &declaration_statement->declaration;

		if (token.type == ':') {
			next_token();
			variable_declaration->type = parse_type();
		}

		/* append multiple variable declarations */
		if (last_statement != NULL) {
			last_statement->next = (statement_t*) declaration_statement;
		} else {
			first_statement = (statement_t*) declaration_statement;
		}
		last_statement = (statement_t*) declaration_statement;

		/* do we have an assignment expression? */
		if (token.type == '=') {
			next_token();
			statement_t *assign = parse_initial_assignment(declaration->symbol);

			last_statement->next = assign;
			last_statement       = assign;
		}

		/* check if we have more declared symbols separated by ',' */
		if (token.type != ',')
			break;
		next_token();
	}

	expect(T_NEWLINE, end_error);

end_error:
	return first_statement;
}

static statement_t *parse_expression_statement(void)
{
	expression_statement_t *expression_statement
		= allocate_ast_zero(sizeof(expression_statement[0]));

	expression_statement->statement.type = STATEMENT_EXPRESSION;
	expression_statement->expression     = parse_expression();
	expect(T_NEWLINE, end_error);

end_error:
	return (statement_t*) expression_statement;
}

static statement_t *parse_newline(void)
{
	eat(T_NEWLINE);

	if (token.type == T_INDENT)
		return parse_block();

	return NULL;
}

static void register_statement_parsers(void)
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
	statement_t       *statement = NULL;
	source_position_t  start     = source_position;

	parse_statement_function parser = NULL;
	if (token.type < ARR_LEN(statement_parsers))
		parser = statement_parsers[token.type];

	add_anchor_token(T_NEWLINE);
	if (parser != NULL) {
		statement = parser();
	} else {
		parse_declaration_function declaration_parser = NULL;
		if (token.type < ARR_LEN(declaration_parsers))
			declaration_parser = declaration_parsers[token.type];

		if (declaration_parser != NULL) {
			declaration_parser();
		} else {
			statement = parse_expression_statement();
		}
	}
	rem_anchor_token(T_NEWLINE);

	if (statement == NULL)
		return NULL;

	statement->source_position = start;
	statement_t *next = statement->next;
	while (next != NULL) {
		next->source_position = start;
		next                  = next->next;
	}

	return statement;
}

static statement_t *parse_block(void)
{
	eat(T_INDENT);

	block_statement_t *block = allocate_ast_zero(sizeof(block[0]));
	block->statement.type    = STATEMENT_BLOCK;

	context_t *last_context = current_context;
	current_context         = &block->context;

	add_anchor_token(T_DEDENT);

	statement_t *last_statement = NULL;
	while (token.type != T_DEDENT) {
		/* parse statement */
		statement_t *statement = parse_statement();
		if (statement == NULL)
			continue;

		if (last_statement != NULL) {
			last_statement->next = statement;
		} else {
			block->statements = statement;
		}
		last_statement = statement;
		/* the parse rule might have produced multiple statements */
		while (last_statement->next != NULL)
			last_statement = last_statement->next;
	}

	assert(current_context == &block->context);
	current_context = last_context;

	block->end_position = source_position;
	rem_anchor_token(T_DEDENT);
	expect(T_DEDENT, end_error);

end_error:
	return (statement_t*) block;
}

static void parse_parameter_declaration(method_type_t *method_type,
                                        method_parameter_t **parameters)
{
	assert(method_type != NULL);

	if (token.type == ')')
		return;

	method_parameter_type_t *last_type = NULL;
	method_parameter_t      *last_param = NULL;
	if (parameters != NULL)
		*parameters = NULL;

	while (true) {
		if (token.type == T_DOTDOTDOT) {
			method_type->variable_arguments = 1;
			next_token();

			if (token.type == ',') {
				parse_error("'...' has to be the last argument in a function "
				            "parameter list");
				eat_until_anchor();
				return;
			}
			break;
		}

		if (token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing parameter",
			                     T_IDENTIFIER, 0);
			eat_until_anchor();
			return;
		}
		symbol_t *symbol = token.v.symbol;
		next_token();

		expect(':', end_error);

		method_parameter_type_t *param_type
			= allocate_ast_zero(sizeof(param_type[0]));
		param_type->type = parse_type();

		if (last_type != NULL) {
			last_type->next = param_type;
		} else {
			method_type->parameter_types = param_type;
		}
		last_type = param_type;

		if (parameters != NULL) {
			method_parameter_t *method_param
				= allocate_ast_zero(sizeof(method_param[0]));
			method_param->declaration.type 
				= DECLARATION_METHOD_PARAMETER;
			method_param->declaration.symbol          = symbol;
			method_param->declaration.source_position = source_position;
			method_param->type                        = param_type->type;

			if (last_param != NULL) {
				last_param->next = method_param;
			} else {
				*parameters = method_param;
			}
			last_param = method_param;
		}

		if (token.type != ',')
			break;
		next_token();
	}

end_error:
	;
}

static type_constraint_t *parse_type_constraints(void)
{
	type_constraint_t *first_constraint = NULL;
	type_constraint_t *last_constraint  = NULL;

	while (token.type == T_IDENTIFIER) {
		type_constraint_t *constraint 
			= allocate_ast_zero(sizeof(constraint[0]));

		constraint->concept_symbol = token.v.symbol;
		next_token();

		if (last_constraint == NULL) {
			first_constraint = constraint;
		} else {
			last_constraint->next = constraint;
		}
		last_constraint = constraint;
	}

	return first_constraint;
}

static type_variable_t *parse_type_parameter(void)
{
	type_variable_t *type_variable
		= allocate_ast_zero(sizeof(type_variable[0]));
	type_variable->declaration.type = DECLARATION_TYPE_VARIABLE;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing type parameter",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return NULL;
	}
	type_variable->declaration.source_position = source_position;
	type_variable->declaration.symbol          = token.v.symbol;
	next_token();

	if (token.type == ':') {
		next_token();
		type_variable->constraints = parse_type_constraints();
	}

	return type_variable;
}

static type_variable_t *parse_type_parameters(context_t *context)
{
	type_variable_t *first_variable = NULL;
	type_variable_t *last_variable  = NULL;
	while (true) {
		type_variable_t *type_variable = parse_type_parameter();

		if (last_variable != NULL) {
			last_variable->next = type_variable;
		} else {
			first_variable = type_variable;
		}
		last_variable = type_variable;

		if (context != NULL) {
			declaration_t *declaration = & type_variable->declaration;
			declaration->next          = context->declarations;
			context->declarations      = declaration;
		}

		if (token.type != ',')
			break;
		next_token();
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

static void parse_method(method_t *method)
{
	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	method_type->type.type     = TYPE_METHOD;

	context_t *last_context = current_context;
	current_context         = &method->context;

	if (token.type == '<') {
		next_token();
		method->type_parameters = parse_type_parameters(current_context);
		expect('>', end_error);
	}

	expect('(', end_error);

	parse_parameter_declaration(method_type, &method->parameters);
	method->type = method_type;

	/* add parameters to context */
	method_parameter_t *parameter = method->parameters;
	while (parameter != NULL) {
		declaration_t *declaration    = (declaration_t*) parameter;
		declaration->next             = current_context->declarations;
		current_context->declarations = declaration;

		parameter = parameter->next;
	}

	expect(')', end_error);

	method_type->result_type = type_void;
	if (token.type == ':') {
		next_token();
		if (token.type == T_NEWLINE) {
			method->statement = parse_sub_block();
			goto method_parser_end;
		}

		method_type->result_type = parse_type();

		if (token.type == ':') {
			next_token();
			method->statement = parse_sub_block();
			goto method_parser_end;
		}
	}
	expect(T_NEWLINE, end_error);

method_parser_end:
	assert(current_context == &method->context);
	current_context = last_context;

end_error:
	;
}

static void parse_method_declaration(void)
{
	eat(T_func);

	method_declaration_t *method_declaration
		= allocate_ast_zero(sizeof(method_declaration[0]));
	method_declaration->declaration.type            = DECLARATION_METHOD;

	if (token.type == T_extern) {
		method_declaration->method.is_extern = 1;
		next_token();
	}

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing function",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	method_declaration->declaration.source_position = source_position;
	method_declaration->declaration.symbol          = token.v.symbol;
	next_token();

	parse_method(&method_declaration->method);

	add_declaration((declaration_t*) method_declaration);
}

static void parse_global_variable(void)
{
	eat(T_var);

	variable_declaration_t *variable = allocate_ast_zero(sizeof(variable[0]));
	variable->declaration.type = DECLARATION_VARIABLE;
	variable->is_global        = 1;

	if (token.type == T_extern) {
		next_token();
		variable->is_extern = 1;
	}

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing global variable",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}

	variable->declaration.source_position = source_position;
	variable->declaration.symbol          = token.v.symbol;
	next_token();

	if (token.type != ':') {
		parse_error_expected("global variables must have a type specified",
		                     ':', 0);
		eat_until_anchor();
	} else {
		next_token();
		variable->type = parse_type();
		expect(T_NEWLINE, end_error);
	}

end_error:
	add_declaration((declaration_t*) variable);
}

static void parse_constant(void)
{
	eat(T_const);

	constant_t *constant       = allocate_ast_zero(sizeof(constant[0]));
	constant->declaration.type = DECLARATION_CONSTANT;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing constant", T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	constant->declaration.source_position = source_position; 
	constant->declaration.symbol          = token.v.symbol;
	next_token();

	if (token.type == ':') {
		next_token();
		constant->type = parse_type();
	}

	expect('=', end_error);
	constant->expression = parse_expression();

	expect(T_NEWLINE, end_error);

end_error:
	add_declaration((declaration_t*) constant);
}

static void parse_typealias(void)
{
	eat(T_typealias);

	typealias_t *typealias      = allocate_ast_zero(sizeof(typealias[0]));
	typealias->declaration.type = DECLARATION_TYPEALIAS;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typealias",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	typealias->declaration.source_position = source_position;
	typealias->declaration.symbol          = token.v.symbol;
	next_token();

	expect('=', end_error);
	typealias->type = parse_type();
	expect(T_NEWLINE, end_error);

end_error:
	add_declaration((declaration_t*) typealias);
}

static attribute_t *parse_attribute(void)
{
	eat('$');

	attribute_t *attribute = NULL;

	if (token.type < 0) {
		parse_error("problem while parsing attribute");
		return NULL;
	}

	parse_attribute_function parser = NULL;
	if (token.type < ARR_LEN(attribute_parsers))
		parser = attribute_parsers[token.type];

	if (parser == NULL) {
		parser_print_error_prefix();
		print_token(stderr, &token);
		fprintf(stderr, " doesn't start a known attribute type\n");
		return NULL;
	}

	if (parser != NULL) {
		attribute = parser();
	}

	return attribute;
}

attribute_t *parse_attributes(void)
{
	attribute_t *last = NULL;

	while (token.type == '$') {
		attribute_t *attribute = parse_attribute();
		if (attribute != NULL) {
			attribute->next = last;
			last = attribute;
		}
	}

	return last;
}

static void parse_class(void)
{
	eat(T_class);

	typealias_t *typealias      = allocate_ast_zero(sizeof(typealias[0]));
	typealias->declaration.type = DECLARATION_TYPEALIAS;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing class",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	typealias->declaration.source_position = source_position;
	typealias->declaration.symbol          = token.v.symbol;
	next_token();

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_CLASS;
	compound_type->symbol     = typealias->declaration.symbol;
	compound_type->attributes = parse_attributes();

	typealias->type = (type_t*) compound_type;

	expect(':', end_error);
	expect(T_NEWLINE, end_error);

	if (token.type == T_INDENT) {
		next_token();

		context_t *last_context = current_context;
		current_context         = &compound_type->context;

		while (token.type != T_EOF && token.type != T_DEDENT) {
			parse_declaration();		
		}
		next_token();

		assert(current_context == &compound_type->context);
		current_context = last_context;
	}

end_error:
	add_declaration((declaration_t*) typealias);
}

static void parse_struct(void)
{
	eat(T_struct);

	typealias_t *typealias      = allocate_ast_zero(sizeof(typealias[0]));
	typealias->declaration.type = DECLARATION_TYPEALIAS;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing struct",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	typealias->declaration.source_position = source_position;
	typealias->declaration.symbol          = token.v.symbol;
	next_token();

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_STRUCT;
	compound_type->symbol     = typealias->declaration.symbol;

	if (token.type == '<') {
		next_token();
		compound_type->type_parameters 
			= parse_type_parameters(&compound_type->context);
		expect('>', end_error);
	}

	compound_type->attributes = parse_attributes();

	typealias->type = (type_t*) compound_type;

	expect(':', end_error);
	expect(T_NEWLINE, end_error);

	if (token.type == T_INDENT) {
		next_token();
		compound_type->entries = parse_compound_entries();
		eat(T_DEDENT);
	}

	add_declaration((declaration_t*) typealias);

end_error:
	;
}

static void parse_union(void)
{
	eat(T_union);

	typealias_t *typealias      = allocate_ast_zero(sizeof(typealias[0]));
	typealias->declaration.type = DECLARATION_TYPEALIAS;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing union",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	typealias->declaration.source_position = source_position;
	typealias->declaration.symbol          = token.v.symbol;
	next_token();

	compound_type_t *compound_type 
		= allocate_ast_zero(sizeof(compound_type[0]));
	compound_type->type.type  = TYPE_COMPOUND_UNION;
	compound_type->symbol     = typealias->declaration.symbol;
	compound_type->attributes = parse_attributes();

	typealias->type = (type_t*) compound_type;

	expect(':', end_error);
	expect(T_NEWLINE, end_error);

	if (token.type == T_INDENT) {
		next_token();
		compound_type->entries = parse_compound_entries();
		eat(T_DEDENT);
	}

end_error:
	add_declaration((declaration_t*) typealias);
}

static concept_method_t *parse_concept_method(void)
{
	expect(T_func, end_error);

	concept_method_t *method = allocate_ast_zero(sizeof(method[0]));
	method->declaration.type = DECLARATION_CONCEPT_METHOD;

	method_type_t *method_type = allocate_type_zero(sizeof(method_type[0]));
	memset(method_type, 0, sizeof(method_type[0]));
	method_type->type.type = TYPE_METHOD;
	
	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing concept method",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		goto end_error;
	}

	method->declaration.source_position = source_position;
	method->declaration.symbol          = token.v.symbol;
	next_token();

	expect('(', end_error);
	parse_parameter_declaration(method_type, &method->parameters);
	expect(')', end_error);

	if (token.type == ':') {
		next_token();
		method_type->result_type = parse_type();
	} else {
		method_type->result_type = type_void;
	}
	expect(T_NEWLINE, end_error);

	method->method_type = method_type;

	add_declaration((declaration_t*) method);

	return method;

end_error:
	return NULL;
}

static void parse_concept(void)
{
	eat(T_concept);

	concept_t *concept        = allocate_ast_zero(sizeof(concept[0]));
	concept->declaration.type = DECLARATION_CONCEPT;
	
	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing concept",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}

	concept->declaration.source_position = source_position;
	concept->declaration.symbol          = token.v.symbol;
	next_token();

	if (token.type == '<') {
		next_token();
		context_t       *context = &concept->context;
		concept->type_parameters = parse_type_parameters(context);
		expect('>', end_error);
	}
	expect(':', end_error);
	expect(T_NEWLINE, end_error);

	if (token.type != T_INDENT) {
		goto end_of_parse_concept;
	}
	next_token();

	concept_method_t *last_method = NULL;
	while (token.type != T_DEDENT) {
		if (token.type == T_EOF) {
			parse_error("EOF while parsing concept");
			goto end_of_parse_concept;
		}

		concept_method_t *method = parse_concept_method();
		method->concept          = concept;

		if (last_method != NULL) {
			last_method->next = method;
		} else {
			concept->methods = method;
		}
		last_method = method;
	}
	next_token();

end_of_parse_concept:
	add_declaration((declaration_t*) concept);
	return;

end_error:
	;
}

static concept_method_instance_t *parse_concept_method_instance(void)
{
	concept_method_instance_t *method_instance
		= allocate_ast_zero(sizeof(method_instance[0]));

	expect(T_func, end_error);
	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing concept method "
		                     "instance", T_IDENTIFIER, 0);
		eat_until_anchor();
		goto end_error;
	}
	method_instance->source_position = source_position;
	method_instance->symbol          = token.v.symbol;
	next_token();

	parse_method(& method_instance->method);
	return method_instance;

end_error:
	return NULL;
}

static void parse_concept_instance(void)
{
	eat(T_instance);

	concept_instance_t *instance = allocate_ast_zero(sizeof(instance[0]));
	instance->source_position    = source_position;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing concept instance",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	instance->concept_symbol = token.v.symbol;
	next_token();

	if (token.type == '<') {
		next_token();
		instance->type_parameters
			= parse_type_parameters(&instance->context);
		expect('>', end_error);
	}

	instance->type_arguments = parse_type_arguments();
	
	expect(':', end_error);
	expect(T_NEWLINE, end_error);

	if (token.type != T_INDENT) {
		goto add_instance;
	}
	eat(T_INDENT);

	concept_method_instance_t *last_method = NULL;
	while (token.type != T_DEDENT) {
		if (token.type == T_EOF) {
			parse_error("EOF while parsing concept instance");
			return;
		}
		if (token.type == T_NEWLINE) {
			next_token();
			continue;
		}

		concept_method_instance_t *method = parse_concept_method_instance();
		if (method == NULL)
			continue;

		if (last_method != NULL) {
			last_method->next = method;
		} else {
			instance->method_instances = method;
		}
		last_method = method;
	}
	eat(T_DEDENT);

add_instance:
	assert(current_context != NULL);
	instance->next                     = current_context->concept_instances;
	current_context->concept_instances = instance;
	return;

end_error:
	;
}

static void skip_declaration(void)
{
	next_token();
}

static void parse_export(void)
{
	eat(T_export);

	while (true) {
		if (token.type == T_NEWLINE) {
			break;
		}
		if (token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing export declaration",
			                     T_IDENTIFIER, 0);
			eat_until_anchor();
			return;
		}

		export_t *export        = allocate_ast_zero(sizeof(export[0]));
		export->symbol          = token.v.symbol;
		export->source_position = source_position;
		next_token();

		assert(current_context != NULL);
		export->next             = current_context->exports;
		current_context->exports = export;

		if (token.type != ',') {
			break;
		}
		next_token();
	}
	expect(T_NEWLINE, end_error);

end_error:
	;
}

void parse_declaration(void)
{
	if (token.type < 0) {
		if (token.type == T_EOF)
			return;

		/* this shouldn't happen if the lexer is correct... */
		parse_error_expected("problem while parsing declaration",
		                     T_DEDENT, 0);
		return;
	}

	parse_declaration_function parser = NULL;
	if (token.type < ARR_LEN(declaration_parsers))
		parser = declaration_parsers[token.type];

	if (parser == NULL) {
		parse_error_expected("Couldn't parse declaration",
		                     T_func, T_var, T_extern, T_struct, T_concept,
		                     T_instance, 0);
		eat_until_anchor();
		return;
	}

	if (parser != NULL) {
		parser();
	}
}

static namespace_t *get_namespace(symbol_t *symbol)
{
	/* search for an existing namespace */
	namespace_t *namespace = namespaces;
	while (namespace != NULL) {
		if (namespace->symbol == symbol)
			return namespace;
	
		namespace = namespace->next;
	}

	namespace         = allocate_ast_zero(sizeof(namespace[0]));
	namespace->symbol = symbol;

	namespace->next = namespaces;
	namespaces      = namespace;

	return namespace;
}

static namespace_t *parse_namespace(void)
{
	symbol_t *namespace_symbol = NULL;

	/* parse namespace name */
	if (token.type == T_namespace) {
		next_token();
		if (token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing namespace", 
			                     T_IDENTIFIER, 0);
			eat_until_anchor();
		}
		namespace_symbol = token.v.symbol;
		next_token();

		if (token.type != T_NEWLINE) {
			parse_error("extra tokens after namespace definition");
			eat_until_anchor();
		} else {
			next_token();
		}
	}	

	namespace_t *namespace = get_namespace(namespace_symbol);
	assert(current_context == NULL);
	current_context        = &namespace->context;

	/* parse namespace entries */
	while (token.type != T_EOF) {
		parse_declaration();
	}

	assert(current_context == &namespace->context);
	current_context = NULL;

	return namespace;
}

static void register_declaration_parsers(void)
{
	register_declaration_parser(parse_method_declaration, T_func);
	register_declaration_parser(parse_global_variable,    T_var);
	register_declaration_parser(parse_constant,           T_const);
	register_declaration_parser(parse_class,              T_class);
	register_declaration_parser(parse_struct,             T_struct);
	register_declaration_parser(parse_union,              T_union);
	register_declaration_parser(parse_typealias,          T_typealias);
	register_declaration_parser(parse_concept,            T_concept);
	register_declaration_parser(parse_concept_instance,   T_instance);
	register_declaration_parser(parse_export,             T_export);
	register_declaration_parser(skip_declaration,         T_NEWLINE);
}

namespace_t *parse(FILE *in, const char *input_name)
{
	memset(token_anchor_set, 0, sizeof(token_anchor_set));

	lexer_init(in, input_name);
	next_token();

	add_anchor_token(T_EOF);

	namespace_t *namespace = parse_namespace();
	namespace->filename    = input_name;

	rem_anchor_token(T_EOF);

#ifndef NDEBUG
	for (int i = 0; i < T_LAST_TOKEN; ++i) {
		if (token_anchor_set[i] > 0) {
			panic("leaked token");
		}
	}
#endif

	lexer_destroy();

	if (error) {
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

