#include <config.h>

#include "token_t.h"

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

static expression_parse_function_t *expression_parsers;
static parse_statement_function    *statement_parsers;
static parse_declaration_function  *declaration_parsers;
static parse_attribute_function    *attribute_parsers;

static unsigned char token_anchor_set[T_LAST_TOKEN];

static symbol_t  *current_module_name;
static context_t *current_context;

static int      error = 0;
       token_t  token;

module_t *modules;

static inline void *allocate_ast_zero(size_t size)
{
	void *res = allocate_ast(size);
	memset(res, 0, size);
	return res;
}

static size_t get_entity_struct_size(entity_kind_t kind)
{
	static const size_t sizes[] = {
		[ENTITY_ERROR]              = sizeof(entity_base_t),
		[ENTITY_FUNCTION]           = sizeof(function_entity_t),
		[ENTITY_FUNCTION_PARAMETER] = sizeof(function_parameter_t),
		[ENTITY_VARIABLE]           = sizeof(variable_t),
		[ENTITY_CONSTANT]           = sizeof(constant_t),
		[ENTITY_TYPE_VARIABLE]      = sizeof(type_variable_t),
		[ENTITY_TYPEALIAS]          = sizeof(typealias_t),
		[ENTITY_CONCEPT]            = sizeof(concept_t),
		[ENTITY_CONCEPT_FUNCTION]   = sizeof(concept_function_t),
		[ENTITY_LABEL]              = sizeof(label_t)
	};
	assert(kind < sizeof(sizes)/sizeof(sizes[0]));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

entity_t *allocate_entity(entity_kind_t kind)
{
	size_t    size   = get_entity_struct_size(kind);
	entity_t *entity = allocate_ast_zero(size);
	entity->kind     = kind;
	return entity;
}

static size_t get_expression_struct_size(expression_kind_t kind)
{
	static const size_t sizes[] = {
		[EXPR_ERROR]         = sizeof(expression_base_t),
		[EXPR_INT_CONST]     = sizeof(int_const_t),
		[EXPR_FLOAT_CONST]   = sizeof(float_const_t),
		[EXPR_BOOL_CONST]    = sizeof(bool_const_t),
		[EXPR_STRING_CONST]  = sizeof(string_const_t),
		[EXPR_NULL_POINTER]  = sizeof(expression_base_t),
		[EXPR_REFERENCE]     = sizeof(reference_expression_t),
		[EXPR_CALL]          = sizeof(call_expression_t),
		[EXPR_SELECT]        = sizeof(select_expression_t),
		[EXPR_ARRAY_ACCESS]  = sizeof(array_access_expression_t),
		[EXPR_SIZEOF]        = sizeof(sizeof_expression_t),
		[EXPR_FUNC]          = sizeof(func_expression_t),
	};
	if (kind >= EXPR_UNARY_FIRST && kind <= EXPR_UNARY_LAST) {
		return sizeof(unary_expression_t);
	}
	if (kind >= EXPR_BINARY_FIRST && kind <= EXPR_BINARY_LAST) {
		return sizeof(binary_expression_t);
	}
	assert(kind <= sizeof(sizes)/sizeof(sizes[0]));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

expression_t *allocate_expression(expression_kind_t kind)
{
	size_t        size       = get_expression_struct_size(kind);
	expression_t *expression = allocate_ast_zero(size);
	expression->kind         = kind;
	return expression;
}

static size_t get_statement_struct_size(statement_kind_t kind)
{
	static const size_t sizes[] = {
		[STATEMENT_ERROR]       = sizeof(statement_base_t),
		[STATEMENT_BLOCK]       = sizeof(block_statement_t),
		[STATEMENT_RETURN]      = sizeof(return_statement_t),
		[STATEMENT_DECLARATION] = sizeof(declaration_statement_t),
		[STATEMENT_IF]          = sizeof(if_statement_t),
		[STATEMENT_EXPRESSION]  = sizeof(expression_statement_t),
		[STATEMENT_GOTO]        = sizeof(goto_statement_t),
		[STATEMENT_LABEL]       = sizeof(label_statement_t),
	};
	assert(kind < sizeof(sizes)/sizeof(sizes[0]));
	assert(sizes[kind] != 0);
	return sizes[kind];
};

static statement_t *allocate_statement(statement_kind_t kind)
{
	size_t       size      = get_statement_struct_size(kind);
	statement_t *statement = allocate_ast_zero(size);
	statement->kind        = kind;
	return statement;
}

static size_t get_type_struct_size(type_kind_t kind)
{
	static const size_t sizes[] = {
		[TYPE_ERROR]                   = sizeof(type_base_t),
		[TYPE_VOID]                    = sizeof(type_base_t),
		[TYPE_ATOMIC]                  = sizeof(atomic_type_t),
		[TYPE_COMPOUND_STRUCT]         = sizeof(compound_type_t),
		[TYPE_COMPOUND_UNION]          = sizeof(compound_type_t),
		[TYPE_FUNCTION]                = sizeof(function_type_t),
		[TYPE_POINTER]                 = sizeof(pointer_type_t),
		[TYPE_ARRAY]                   = sizeof(array_type_t),
		[TYPE_TYPEOF]                  = sizeof(typeof_type_t),
		[TYPE_REFERENCE]               = sizeof(type_reference_t),
		[TYPE_REFERENCE_TYPE_VARIABLE] = sizeof(type_reference_t),
		[TYPE_BIND_TYPEVARIABLES]      = sizeof(bind_typevariables_type_t)
	};
	assert(kind < sizeof(sizes)/sizeof(sizes[0]));
	assert(sizes[kind] != 0);
	return sizes[kind];
}

type_t *allocate_type(type_kind_t kind)
{
	size_t  size = get_type_struct_size(kind);
	type_t *type = obstack_alloc(type_obst, size);
	memset(type, 0, size);
	type->kind = kind;
	return type;
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

static void add_anchor_token(token_type_t token_type)
{
	assert(token_type < T_LAST_TOKEN);
	++token_anchor_set[token_type];
}

static void rem_anchor_token(token_type_t token_type)
{
	assert(token_type < T_LAST_TOKEN);
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
static void eat_until_matching_token(token_type_t type)
{
	token_type_t end_token;
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
		if (UNLIKELY(token.type != (expected))) {          \
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

static void parse_function(function_t *function);

static statement_t *parse_block(void);

static void parse_parameter_declarations(function_type_t *function_type,
                                         function_parameter_t **parameters);

static atomic_type_kind_t parse_unsigned_atomic_type(void)
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
		parse_error_expected("couldn't parse type", T_byte, T_short, T_int,
		                     T_long, 0);
		return ATOMIC_TYPE_INVALID;
	}
}

static atomic_type_kind_t parse_signed_atomic_type(void)
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
		parse_error_expected("couldn't parse type", T_byte, T_short, T_int,
		                     T_long, T_float, T_double, 0);
		return ATOMIC_TYPE_INVALID;
	}
}

static type_t *parse_atomic_type(void)
{
	atomic_type_kind_t akind;

	switch (token.type) {
	case T_unsigned:
		next_token();
		akind = parse_unsigned_atomic_type();
		break;
	case T_signed:
		next_token();
		/* fallthrough */
	default:
		akind = parse_signed_atomic_type();
		break;
	}

	type_t *type = allocate_type(TYPE_ATOMIC);
	type->atomic.akind = akind;

	type_t *result = typehash_insert(type);
	if (result != type) {
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
	type_t *type = allocate_type(TYPE_TYPEOF);

	eat(T_typeof);
	expect('(', end_error);
	add_anchor_token(')');
	type->typeof.expression = parse_expression();
	rem_anchor_token(')');
	expect(')', end_error);

end_error:
	return type;
}

static type_t *parse_type_ref(void)
{
	assert(token.type == T_IDENTIFIER);

	type_t *type = allocate_type(TYPE_REFERENCE);
	type->reference.symbol          = token.v.symbol;
	type->reference.source_position = source_position;
	next_token();

	if (token.type == '<') {
		next_token();
		add_anchor_token('>');
		type->reference.type_arguments = parse_type_arguments();
		rem_anchor_token('>');
		expect('>', end_error);
	}

end_error:
	return type;
}

static type_t *create_error_type(void)
{
	return allocate_type(TYPE_ERROR);
}

static type_t *parse_function_type(void)
{
	eat(T_func);

	type_t *type = allocate_type(TYPE_FUNCTION);

	parse_parameter_declarations(&type->function, NULL);
	expect(':', end_error);
	type->function.result_type = parse_type();

	return type;

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
			eat_until_matching_token(T_NEWLINE);
			next_token();
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

	type_t *type = allocate_type(TYPE_COMPOUND_UNION);
	type->compound.attributes = parse_attributes();

	expect(':', end_error);
	expect(T_NEWLINE, end_error);
	expect(T_INDENT, end_error);

	add_anchor_token(T_DEDENT);
	type->compound.entries = parse_compound_entries();

	/* force end of statement */
	rem_anchor_token(T_DEDENT);
	assert(token.type == T_DEDENT);
	replace_token_type(T_NEWLINE);

end_error:
	return type;
}

static type_t *parse_struct_type(void)
{
	eat(T_struct);

	type_t *type = allocate_type(TYPE_COMPOUND_STRUCT);
	type->compound.attributes = parse_attributes();

	expect(':', end_error);
	expect(T_NEWLINE, end_error);
	expect(T_INDENT, end_error);

	add_anchor_token(T_DEDENT);
	type->compound.entries = parse_compound_entries();

	/* force end of statement */
	rem_anchor_token(T_DEDENT);
	assert(token.type == T_DEDENT);
	replace_token_type(T_NEWLINE);

end_error:
	return type;
}

static type_t *make_pointer_type_no_hash(type_t *points_to)
{
	type_t *type = allocate_type(TYPE_POINTER);
	type->pointer.points_to = points_to;

	return type;
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
		type = parse_function_type();
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
			expression_t *size = parse_expression();
			rem_anchor_token(']');
			expect(']', end_error);

			type_t *array_type = allocate_type(TYPE_ARRAY);
			array_type->array.element_type    = type;
			array_type->array.size_expression = size;
			type = array_type;

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
	expression_t *expression       = allocate_expression(EXPR_STRING_CONST);
	expression->string_const.value = token.v.string;
	next_token();

	return expression;
}

static expression_t *parse_int_const(void)
{
	expression_t *expression    = allocate_expression(EXPR_INT_CONST);
	expression->int_const.value = token.v.intvalue;
	next_token();

	return expression;
}

static expression_t *parse_true(void)
{
	eat(T_true);
	expression_t *expression     = allocate_expression(EXPR_BOOL_CONST);
	expression->bool_const.value = true;

	return expression;
}

static expression_t *parse_false(void)
{
	eat(T_false);
	expression_t *expression     = allocate_expression(EXPR_BOOL_CONST);
	expression->bool_const.value = false;

	return expression;
}

static expression_t *parse_null(void)
{
	eat(T_null);
	expression_t *expression = allocate_expression(EXPR_NULL_POINTER);
	expression->base.type    = make_pointer_type(type_void);

	return expression;
}

static expression_t *parse_func_expression(void)
{
	eat(T_func);
	expression_t *expression = allocate_expression(EXPR_FUNC);
	parse_function(&expression->func.function);

	return expression;
}

static expression_t *parse_reference(void)
{
	expression_t *expression     = allocate_expression(EXPR_REFERENCE);
	expression->reference.symbol = token.v.symbol;
	next_token();

	if (token.type == T_TYPESTART) {
		next_token();
		add_anchor_token('>');
		expression->reference.type_arguments = parse_type_arguments();
		rem_anchor_token('>');
		expect('>', end_error);
	}

end_error:
	return expression;
}

static expression_t *create_error_expression(void)
{
	expression_t *expression = allocate_expression(EXPR_ERROR);
	expression->base.type    = create_error_type();
	return expression;
}

static expression_t *parse_sizeof(void)
{
	eat(T_sizeof);
	expression_t *expression = allocate_expression(EXPR_SIZEOF);

	if (token.type == '(') {
		next_token();
		type_t *type = allocate_type(TYPE_TYPEOF);
		add_anchor_token(')');
		type->typeof.expression = parse_expression();
		rem_anchor_token(')');
		expect(')', end_error);

		expression->sizeofe.type = type;
	} else {
		expect('<', end_error);
		add_anchor_token('>');
		expression->sizeofe.type = parse_type();
		rem_anchor_token('>');
		expect('>', end_error);
	}
	return expression;

end_error:
	return create_error_expression();
}

void register_statement_parser(parse_statement_function parser,
                               token_type_t token_type)
{
	unsigned len = (unsigned) ARR_LEN(statement_parsers);
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
                                 token_type_t token_type)
{
	unsigned len = (unsigned) ARR_LEN(declaration_parsers);
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

void register_attribute_parser(parse_attribute_function parser,
                               token_type_t token_type)
{
	unsigned len = (unsigned) ARR_LEN(attribute_parsers);
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

static expression_parse_function_t *get_expression_parser_entry(token_type_t token_type)
{
	unsigned len = (unsigned) ARR_LEN(expression_parsers);
	if (token_type >= len) {
		ARR_RESIZE(expression_parse_function_t, expression_parsers, token_type + 1);
		memset(& expression_parsers[len], 0,
				(token_type - len + 1) * sizeof(expression_parsers[0]));
	}

	return &expression_parsers[token_type];
}

void register_expression_parser(parse_expression_function parser,
                                token_type_t token_type)
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
                                      token_type_t token_type,
									  unsigned precedence)
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

	return create_error_expression();
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

	expression_t *expression = allocate_expression(EXPR_UNARY_CAST);
	
	expect('<', end_error);
	expression->base.type = parse_type();
	expect('>', end_error);

	expression->unary.value = parse_sub_expression(PREC_CAST);

end_error:
	return expression;
}

static expression_t *parse_call_expression(expression_t *left)
{
	expression_t *expression  = allocate_expression(EXPR_CALL);
	expression->call.function = left;

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
				expression->call.arguments = argument;
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
	return expression;
}

static expression_t *parse_select_expression(expression_t *compound)
{
	eat('.');
	expression_t *expression    = allocate_expression(EXPR_SELECT);
	expression->select.compound = compound;

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing compound select",
		                     T_IDENTIFIER, 0);
		return NULL;
	}
	expression->select.symbol = token.v.symbol;
	next_token();

	return expression;
}

static expression_t *parse_array_expression(expression_t *array_ref)
{
	eat('[');
	expression_t *expression           = allocate_expression(EXPR_ARRAY_ACCESS);
	expression->array_access.array_ref = array_ref;
	expression->array_access.index     = parse_expression();

	if (token.type != ']') {
		parse_error_expected("Problem while parsing array access", ']', 0);
		return NULL;
	}
	next_token();

	return expression;
}

#define CREATE_UNARY_EXPRESSION_PARSER(token_type, unexpression_type)     \
static expression_t *parse_##unexpression_type(void)                      \
{                                                                         \
	eat(token_type);                                                      \
	expression_t *expression = allocate_expression(unexpression_type);    \
	expression->unary.value  = parse_sub_expression(PREC_UNARY);          \
                                                                          \
	return expression;                                                    \
}

CREATE_UNARY_EXPRESSION_PARSER('-',          EXPR_UNARY_NEGATE)
CREATE_UNARY_EXPRESSION_PARSER('!',          EXPR_UNARY_NOT)
CREATE_UNARY_EXPRESSION_PARSER('~',          EXPR_UNARY_BITWISE_NOT)
CREATE_UNARY_EXPRESSION_PARSER('*',          EXPR_UNARY_DEREFERENCE)
CREATE_UNARY_EXPRESSION_PARSER('&',          EXPR_UNARY_TAKE_ADDRESS)
CREATE_UNARY_EXPRESSION_PARSER(T_PLUSPLUS,   EXPR_UNARY_INCREMENT)
CREATE_UNARY_EXPRESSION_PARSER(T_MINUSMINUS, EXPR_UNARY_DECREMENT)

#define CREATE_BINEXPR_PARSER_RL(token_type, binexpression_type, prec_r)  \
static expression_t *parse_##binexpression_type(expression_t *left)       \
{                                                                         \
	eat(token_type);                                                      \
	expression_t *expression = allocate_expression(binexpression_type);   \
	expression->binary.left  = left;                                      \
	expression->binary.right = parse_sub_expression(prec_r);              \
                                                                          \
	return expression;                                                    \
}

#define CREATE_BINEXPR_PARSER_LR(token_type, binexpression_type, prec_r)  \
	CREATE_BINEXPR_PARSER_RL(token_type, binexpression_type, prec_r+1)

CREATE_BINEXPR_PARSER_LR('*', EXPR_BINARY_MUL, PREC_MULTIPLICATIVE);
CREATE_BINEXPR_PARSER_LR('/', EXPR_BINARY_DIV, PREC_MULTIPLICATIVE);
CREATE_BINEXPR_PARSER_LR('%', EXPR_BINARY_MOD, PREC_MULTIPLICATIVE);
CREATE_BINEXPR_PARSER_LR('+', EXPR_BINARY_ADD, PREC_ADDITIVE);
CREATE_BINEXPR_PARSER_LR('-', EXPR_BINARY_SUB, PREC_ADDITIVE);
CREATE_BINEXPR_PARSER_LR('<', EXPR_BINARY_LESS, PREC_RELATIONAL);
CREATE_BINEXPR_PARSER_LR('>', EXPR_BINARY_GREATER, PREC_RELATIONAL);
CREATE_BINEXPR_PARSER_LR(T_EQUALEQUAL, EXPR_BINARY_EQUAL, PREC_EQUALITY);
CREATE_BINEXPR_PARSER_RL('=', EXPR_BINARY_ASSIGN, PREC_ASSIGNMENT);
CREATE_BINEXPR_PARSER_LR(T_SLASHEQUAL, EXPR_BINARY_NOTEQUAL, PREC_EQUALITY);
CREATE_BINEXPR_PARSER_LR(T_LESSEQUAL, EXPR_BINARY_LESSEQUAL, PREC_RELATIONAL);
CREATE_BINEXPR_PARSER_LR(T_GREATEREQUAL, EXPR_BINARY_GREATEREQUAL, PREC_RELATIONAL);
CREATE_BINEXPR_PARSER_LR('&', EXPR_BINARY_AND, PREC_AND);
CREATE_BINEXPR_PARSER_LR('|', EXPR_BINARY_OR, PREC_OR);
CREATE_BINEXPR_PARSER_LR('^', EXPR_BINARY_XOR, PREC_XOR);
CREATE_BINEXPR_PARSER_LR(T_ANDAND, EXPR_BINARY_LAZY_AND, PREC_LAZY_AND);
CREATE_BINEXPR_PARSER_LR(T_PIPEPIPE, EXPR_BINARY_LAZY_OR, PREC_LAZY_OR);
CREATE_BINEXPR_PARSER_LR(T_LESSLESS, EXPR_BINARY_SHIFTLEFT, PREC_MULTIPLICATIVE);
CREATE_BINEXPR_PARSER_LR(T_GREATERGREATER, EXPR_BINARY_SHIFTRIGHT, PREC_MULTIPLICATIVE);

static void register_expression_parsers(void)
{
	register_expression_infix_parser(parse_EXPR_BINARY_MUL,       '*', PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_EXPR_BINARY_DIV,       '/', PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_EXPR_BINARY_MOD,       '%', PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_EXPR_BINARY_SHIFTLEFT,
	                                 T_LESSLESS, PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_EXPR_BINARY_SHIFTRIGHT,
	                           T_GREATERGREATER, PREC_MULTIPLICATIVE);
	register_expression_infix_parser(parse_EXPR_BINARY_ADD,       '+',         PREC_ADDITIVE);
	register_expression_infix_parser(parse_EXPR_BINARY_SUB,       '-',         PREC_ADDITIVE);
	register_expression_infix_parser(parse_EXPR_BINARY_LESS,      '<',         PREC_RELATIONAL);
	register_expression_infix_parser(parse_EXPR_BINARY_GREATER,   '>',         PREC_RELATIONAL);
	register_expression_infix_parser(parse_EXPR_BINARY_LESSEQUAL, T_LESSEQUAL, PREC_RELATIONAL);
	register_expression_infix_parser(parse_EXPR_BINARY_GREATEREQUAL,
	                           T_GREATEREQUAL, PREC_RELATIONAL);
	register_expression_infix_parser(parse_EXPR_BINARY_EQUAL,    T_EQUALEQUAL, PREC_EQUALITY);
	register_expression_infix_parser(parse_EXPR_BINARY_NOTEQUAL, T_SLASHEQUAL, PREC_EQUALITY);
	register_expression_infix_parser(parse_EXPR_BINARY_AND,      '&',          PREC_AND);
	register_expression_infix_parser(parse_EXPR_BINARY_LAZY_AND, T_ANDAND,     PREC_LAZY_AND);
	register_expression_infix_parser(parse_EXPR_BINARY_XOR,      '^',          PREC_XOR);
	register_expression_infix_parser(parse_EXPR_BINARY_OR,       '|',          PREC_OR);
	register_expression_infix_parser(parse_EXPR_BINARY_LAZY_OR,  T_PIPEPIPE,   PREC_LAZY_OR);
	register_expression_infix_parser(parse_EXPR_BINARY_ASSIGN,   '=',          PREC_ASSIGNMENT);

	register_expression_infix_parser(parse_array_expression,  '[', PREC_POSTFIX);

	register_expression_infix_parser(parse_call_expression,   '(', PREC_POSTFIX);
	register_expression_infix_parser(parse_select_expression, '.', PREC_POSTFIX);

	register_expression_parser(parse_EXPR_UNARY_NEGATE,           '-');
	register_expression_parser(parse_EXPR_UNARY_NOT,              '!');
	register_expression_parser(parse_EXPR_UNARY_BITWISE_NOT,      '~');
	register_expression_parser(parse_EXPR_UNARY_INCREMENT,        T_PLUSPLUS);
	register_expression_parser(parse_EXPR_UNARY_DECREMENT,        T_MINUSMINUS);

	register_expression_parser(parse_EXPR_UNARY_DEREFERENCE,      '*');
	register_expression_parser(parse_EXPR_UNARY_TAKE_ADDRESS,     '&');
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
	if (token.type == T_ERROR) {
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
	left->base.source_position = start;

	while (true) {
		if (token.type == T_ERROR) {
			return expected_expression_error();
		}

		parser = &expression_parsers[token.type];
		if (parser->infix_parser == NULL)
			break;
		if (parser->infix_precedence < precedence)
			break;

		left = parser->infix_parser(left);
		assert(left != NULL);
		left->base.source_position = start;
	}

	return left;
}

expression_t *parse_expression(void)
{
	return parse_sub_expression(1);
}

static statement_t *parse_return_statement(void)
{
	eat(T_return);

	statement_t *return_statement = allocate_statement(STATEMENT_RETURN);
	if (token.type != T_NEWLINE) {
		return_statement->returns.value = parse_expression();
	}
	expect(T_NEWLINE, end_error);

end_error:
	return return_statement;
}

static statement_t *create_error_statement(void)
{
	return allocate_statement(STATEMENT_ERROR);
}

static statement_t *parse_goto_statement(void)
{
	eat(T_goto);

	statement_t *goto_statement = allocate_statement(STATEMENT_GOTO);
	if (token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing goto statement",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		goto end_error;
	}
	goto_statement->gotos.label_symbol = token.v.symbol;
	next_token();

	expect(T_NEWLINE, end_error);

	return goto_statement;

end_error:
	return create_error_statement();
}

static statement_t *parse_label_statement(void)
{
	eat(':');

	statement_t *label = allocate_statement(STATEMENT_LABEL);
	if (token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing label", T_IDENTIFIER, 0);
		eat_until_anchor();
		goto end_error;
	}
	label->label.label.base.kind            = ENTITY_LABEL;
	label->label.label.base.source_position = source_position;
	label->label.label.base.symbol          = token.v.symbol;
	next_token();

	add_entity((entity_t*) &label->label.label);

	expect(T_NEWLINE, end_error);

	return label;

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
		statement_t *block = allocate_statement(STATEMENT_BLOCK);
		return block;
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

	statement_t *if_statement = allocate_statement(STATEMENT_IF);
	if_statement->ifs.condition       = condition;
	if_statement->ifs.true_statement  = true_statement;
	if_statement->ifs.false_statement = false_statement;

	return if_statement;

end_error:
	return create_error_statement();
}

static statement_t *parse_initial_assignment(symbol_t *symbol)
{
	expression_t *expression     = allocate_expression(EXPR_REFERENCE);
	expression->reference.symbol = symbol;

	expression_t *assign         = allocate_expression(EXPR_BINARY_ASSIGN);
	assign->base.source_position = source_position;
	assign->binary.left          = expression;
	assign->binary.right         = parse_expression();

	statement_t *expr_statement = allocate_statement(STATEMENT_EXPRESSION);
	expr_statement->expression.expression = assign;
	return expr_statement;
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

		statement_t *statement = allocate_statement(STATEMENT_DECLARATION);

		entity_t *entity = (entity_t*) &statement->declaration.entity;
		symbol_t *symbol = token.v.symbol;
		entity->base.kind            = ENTITY_VARIABLE;
		entity->base.source_position = source_position;
		entity->base.symbol          = symbol;
		next_token();

		add_entity(entity);

		if (token.type == ':') {
			next_token();
			entity->variable.type = parse_type();
		}

		/* append multiple variable declarations */
		if (last_statement != NULL) {
			last_statement->base.next = statement;
		} else {
			first_statement = statement;
		}
		last_statement = statement;

		/* do we have an assignment expression? */
		if (token.type == '=') {
			next_token();
			statement_t *assign = parse_initial_assignment(symbol);

			last_statement->base.next = assign;
			last_statement            = assign;
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
	statement_t *statement = allocate_statement(STATEMENT_EXPRESSION);
	statement->expression.expression = parse_expression();
	expect(T_NEWLINE, end_error);

end_error:
	return statement;
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

	statement->base.source_position = start;
	statement_t *next = statement->base.next;
	while (next != NULL) {
		next->base.source_position = start;
		next                       = next->base.next;
	}

	return statement;
}

static statement_t *parse_block(void)
{
	eat(T_INDENT);

	statement_t *block_statement = allocate_statement(STATEMENT_BLOCK);

	context_t *last_context = current_context;
	current_context         = &block_statement->block.context;

	add_anchor_token(T_DEDENT);

	statement_t *last_statement = NULL;
	while (token.type != T_DEDENT) {
		/* parse statement */
		statement_t *statement = parse_statement();
		if (statement == NULL)
			continue;

		if (last_statement != NULL) {
			last_statement->base.next = statement;
		} else {
			block_statement->block.statements = statement;
		}
		last_statement = statement;
		/* the parse rule might have produced multiple statements */
		while (last_statement->base.next != NULL)
			last_statement = last_statement->base.next;
	}

	assert(current_context == &block_statement->block.context);
	current_context = last_context;

	block_statement->block.end_position = source_position;
	rem_anchor_token(T_DEDENT);
	expect(T_DEDENT, end_error);

end_error:
	return block_statement;
}

static void parse_parameter_declarations(function_type_t *function_type,
                                         function_parameter_t **parameters)
{
	assert(function_type != NULL);

	function_parameter_type_t *last_type      = NULL;
	function_parameter_t      *last_parameter = NULL;
	if (parameters != NULL)
		*parameters = NULL;

	expect('(', end_error2);
	if (token.type == ')') {
		next_token();
		return;
	}

	add_anchor_token(')');
	add_anchor_token(',');
	while (true) {
		if (token.type == T_DOTDOTDOT) {
			function_type->variable_arguments = 1;
			next_token();

			if (token.type == ',') {
				parse_error("'...' has to be the last argument in a function "
				            "parameter list");
				eat_until_anchor();
				goto end_error;
			}
			break;
		}

		if (token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing parameter",
			                     T_IDENTIFIER, 0);
			eat_until_anchor();
			goto end_error;
		}
		symbol_t *symbol = token.v.symbol;
		next_token();

		expect(':', end_error);

		function_parameter_type_t *param_type
			= allocate_ast_zero(sizeof(param_type[0]));
		param_type->type = parse_type();

		if (last_type != NULL) {
			last_type->next = param_type;
		} else {
			function_type->parameter_types = param_type;
		}
		last_type = param_type;

		if (parameters != NULL) {
			entity_t *entity = allocate_entity(ENTITY_FUNCTION_PARAMETER);
			entity->base.kind            = ENTITY_FUNCTION_PARAMETER;
			entity->base.symbol          = symbol;
			entity->base.source_position = source_position;
			entity->parameter.type       = param_type->type;

			if (last_parameter != NULL) {
				last_parameter->next = &entity->parameter;
			} else {
				*parameters = &entity->parameter;
			}
			last_parameter = &entity->parameter;
		}

		if (token.type != ',')
			break;
		next_token();
	}

	rem_anchor_token(',');
	rem_anchor_token(')');
	expect(')', end_error2);
	return;

end_error:
	rem_anchor_token(',');
	rem_anchor_token(')');

end_error2:
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

static entity_t *parse_type_parameter(void)
{
	entity_t *entity = allocate_entity(ENTITY_TYPE_VARIABLE);

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("problem while parsing type parameter",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return NULL;
	}
	entity->base.source_position = source_position;
	entity->base.symbol          = token.v.symbol;
	next_token();

	if (token.type == ':') {
		next_token();
		entity->type_variable.constraints = parse_type_constraints();
	}

	return entity;
}

static type_variable_t *parse_type_parameters(context_t *context)
{
	entity_t *first_variable = NULL;
	entity_t *last_variable  = NULL;
	while (true) {
		entity_t *type_variable = parse_type_parameter();

		if (last_variable != NULL) {
			last_variable->type_variable.next = &type_variable->type_variable;
		} else {
			first_variable = type_variable;
		}
		last_variable = type_variable;

		if (context != NULL) {
			type_variable->base.next = context->entities;
			context->entities        = type_variable;
		}

		if (token.type != ',')
			break;
		next_token();
	}

	return &first_variable->type_variable;
}

void add_entity(entity_t *entity)
{
	assert(entity != NULL);
	assert(entity->base.source_position.input_name != NULL);
	assert(current_context != NULL);

	entity->base.next         = current_context->entities;
	current_context->entities = entity;
}

static void parse_function(function_t *function)
{
	type_t *type = allocate_type(TYPE_FUNCTION);

	context_t *last_context = current_context;
	current_context         = &function->context;

	if (token.type == '<') {
		next_token();
		add_anchor_token('>');
		function->type_parameters = parse_type_parameters(current_context);
		rem_anchor_token('>');
		expect('>', end_error);
	}

	parse_parameter_declarations(&type->function, &function->parameters);
	function->type = &type->function;

	/* add parameters to context */
	function_parameter_t *parameter = function->parameters;
	for ( ; parameter != NULL; parameter = parameter->next) {
		add_entity((entity_t*) parameter);
	}

	type->function.result_type = type_void;
	if (token.type == ':') {
		next_token();
		if (token.type == T_NEWLINE) {
			function->statement = parse_sub_block();
			goto function_parser_end;
		}

		type->function.result_type = parse_type();

		if (token.type == ':') {
			next_token();
			function->statement = parse_sub_block();
			goto function_parser_end;
		}
	}
	expect(T_NEWLINE, end_error);

function_parser_end:
	assert(current_context == &function->context);
	current_context = last_context;

end_error:
	;
}

static void parse_function_declaration(void)
{
	eat(T_func);

	entity_t *declaration = allocate_entity(ENTITY_FUNCTION);

	if (token.type == T_extern) {
		declaration->function.function.is_extern = true;
		next_token();
	}

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing function",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	declaration->base.source_position = source_position;
	declaration->base.symbol          = token.v.symbol;
	next_token();

	parse_function(&declaration->function.function);

	add_entity(declaration);
}

static void parse_global_variable(void)
{
	eat(T_var);

	entity_t *declaration = allocate_entity(ENTITY_VARIABLE);
	declaration->variable.is_global = true;

	if (token.type == T_extern) {
		next_token();
		declaration->variable.is_extern = true;
	}

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing global variable",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}

	declaration->base.source_position = source_position;
	declaration->base.symbol          = token.v.symbol;
	next_token();

	if (token.type != ':') {
		parse_error_expected("global variables must have a type specified",
		                     ':', 0);
		eat_until_anchor();
	} else {
		next_token();
		declaration->variable.type = parse_type();
		expect(T_NEWLINE, end_error);
	}

end_error:
	add_entity(declaration);
}

static void parse_constant(void)
{
	eat(T_const);

	entity_t *declaration = allocate_entity(ENTITY_CONSTANT);

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing constant", T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	declaration->base.source_position = source_position;
	declaration->base.symbol          = token.v.symbol;
	next_token();

	if (token.type == ':') {
		next_token();
		declaration->constant.type = parse_type();
	}

	expect('=', end_error);
	declaration->constant.expression = parse_expression();

	expect(T_NEWLINE, end_error);

end_error:
	add_entity(declaration);
}

static void parse_typealias(void)
{
	eat(T_typealias);

	entity_t *declaration = allocate_entity(ENTITY_TYPEALIAS);

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing typealias",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	declaration->base.source_position = source_position;
	declaration->base.symbol          = token.v.symbol;
	next_token();

	expect('=', end_error);
	declaration->typealias.type = parse_type();
	expect(T_NEWLINE, end_error);

end_error:
	add_entity(declaration);
}

static attribute_t *parse_attribute(void)
{
	eat('$');

	attribute_t *attribute = NULL;

	if (token.type == T_ERROR) {
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

static void parse_struct(void)
{
	eat(T_struct);

	entity_t *declaration = allocate_entity(ENTITY_TYPEALIAS);

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing struct",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	declaration->base.source_position = source_position;
	declaration->base.symbol          = token.v.symbol;
	next_token();

	type_t *type = allocate_type(TYPE_COMPOUND_STRUCT);
	type->compound.symbol = declaration->base.symbol;

	if (token.type == '<') {
		next_token();
		type->compound.type_parameters
			= parse_type_parameters(&type->compound.context);
		expect('>', end_error);
	}

	type->compound.attributes = parse_attributes();

	declaration->typealias.type = type;

	expect(':', end_error);
	expect(T_NEWLINE, end_error);

	if (token.type == T_INDENT) {
		next_token();
		type->compound.entries = parse_compound_entries();
		eat(T_DEDENT);
	}

	add_entity(declaration);

end_error:
	;
}

static void parse_union(void)
{
	eat(T_union);

	entity_t *declaration = allocate_entity(ENTITY_TYPEALIAS);

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing union",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}
	declaration->base.source_position = source_position;
	declaration->base.symbol          = token.v.symbol;
	next_token();

	type_t *type = allocate_type(TYPE_COMPOUND_UNION);
	type->compound.symbol     = declaration->base.symbol;
	type->compound.attributes = parse_attributes();

	declaration->typealias.type = type;

	expect(':', end_error);
	expect(T_NEWLINE, end_error);

	if (token.type == T_INDENT) {
		next_token();
		type->compound.entries = parse_compound_entries();
		eat(T_DEDENT);
	}

end_error:
	add_entity(declaration);
}

static concept_function_t *parse_concept_function(void)
{
	expect(T_func, end_error);

	entity_t *declaration
		= allocate_entity(ENTITY_CONCEPT_FUNCTION);

	type_t *type = allocate_type(TYPE_FUNCTION);

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing concept function",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		goto end_error;
	}

	declaration->base.source_position = source_position;
	declaration->base.symbol          = token.v.symbol;
	next_token();

	parse_parameter_declarations(&type->function,
	                             &declaration->concept_function.parameters);

	if (token.type == ':') {
		next_token();
		type->function.result_type = parse_type();
	} else {
		type->function.result_type = type_void;
	}
	expect(T_NEWLINE, end_error);

	declaration->concept_function.type = &type->function;

	add_entity(declaration);

	return &declaration->concept_function;

end_error:
	return NULL;
}

static void parse_concept(void)
{
	eat(T_concept);

	entity_t *declaration = allocate_entity(ENTITY_CONCEPT);

	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing concept",
		                     T_IDENTIFIER, 0);
		eat_until_anchor();
		return;
	}

	declaration->base.source_position = source_position;
	declaration->base.symbol          = token.v.symbol;
	next_token();

	if (token.type == '<') {
		next_token();
		context_t *context                   = &declaration->concept.context;
		add_anchor_token('>');
		declaration->concept.type_parameters = parse_type_parameters(context);
		rem_anchor_token('>');
		expect('>', end_error);
	}
	expect(':', end_error);
	expect(T_NEWLINE, end_error);

	if (token.type != T_INDENT) {
		goto end_of_parse_concept;
	}
	next_token();

	concept_function_t *last_function = NULL;
	while (token.type != T_DEDENT) {
		if (token.type == T_EOF) {
			parse_error("EOF while parsing concept");
			goto end_of_parse_concept;
		}

		concept_function_t *function = parse_concept_function();
		function->concept            = &declaration->concept;

		if (last_function != NULL) {
			last_function->next = function;
		} else {
			declaration->concept.functions = function;
		}
		last_function = function;
	}
	next_token();

end_of_parse_concept:
	add_entity(declaration);
	return;

end_error:
	;
}

static concept_function_instance_t *parse_concept_function_instance(void)
{
	concept_function_instance_t *function_instance
		= allocate_ast_zero(sizeof(function_instance[0]));

	expect(T_func, end_error);
	if (token.type != T_IDENTIFIER) {
		parse_error_expected("Problem while parsing concept function "
		                     "instance", T_IDENTIFIER, 0);
		eat_until_anchor();
		goto end_error;
	}
	function_instance->source_position = source_position;
	function_instance->symbol          = token.v.symbol;
	next_token();

	parse_function(&function_instance->function);
	return function_instance;

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

	concept_function_instance_t *last_function = NULL;
	while (token.type != T_DEDENT) {
		if (token.type == T_EOF) {
			parse_error("EOF while parsing concept instance");
			return;
		}
		if (token.type == T_NEWLINE) {
			next_token();
			continue;
		}

		concept_function_instance_t *function = parse_concept_function_instance();
		if (function == NULL)
			continue;

		if (last_function != NULL) {
			last_function->next = function;
		} else {
			instance->function_instances = function;
		}
		last_function = function;
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

static void parse_import(void)
{
	eat(T_import);
	if (token.type != T_STRING_LITERAL) {
		parse_error_expected("problem while parsing import directive",
		                     T_STRING_LITERAL, 0);
		eat_until_anchor();
		return;
	}
	symbol_t *modulename = symbol_table_insert(token.v.string);
	next_token();

	while (true) {
		if (token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing import directive",
			                     T_IDENTIFIER, 0);
			eat_until_anchor();
			return;
		}

		import_t *import        = allocate_ast_zero(sizeof(import[0]));
		import->module          = modulename;
		import->symbol          = token.v.symbol;
		import->source_position = source_position;

		import->next = current_context->imports;
		current_context->imports = import;
		next_token();

		if (token.type != ',')
			break;
		eat(',');
	}
	expect(T_NEWLINE, end_error);

end_error:
	;
}

static void parse_export(void)
{
	eat(T_export);

	while (true) {
		if (token.type == T_NEWLINE) {
			break;
		}
		if (token.type != T_IDENTIFIER) {
			parse_error_expected("problem while parsing export directive",
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

static void parse_module(void)
{
	eat(T_module);

	/* a simple URL string without a protocol */
	if (token.type != T_STRING_LITERAL) {
		parse_error_expected("problem while parsing module", T_STRING_LITERAL, 0);
		return;
	}

	symbol_t *new_module_name = symbol_table_insert(token.v.string);
	next_token();

	if (current_module_name != NULL && current_module_name != new_module_name) {
		parser_print_error_prefix();
		fprintf(stderr, "new module name '%s' overrides old name '%s'\n",
		        new_module_name->string, current_module_name->string);
	}
	current_module_name = new_module_name;

	expect(T_NEWLINE, end_error);

end_error:
	;
}

void parse_declaration(void)
{
	if (token.type == T_EOF)
		return;

	if (token.type == T_ERROR) {
		/* this shouldn't happen if the lexer is correct... */
		parse_error_expected("problem while parsing declaration",
		                     T_DEDENT, 0);
		return;
	}

	parse_declaration_function parse = NULL;
	if (token.type < ARR_LEN(declaration_parsers))
		parse = declaration_parsers[token.type];

	if (parse == NULL) {
		parse_error_expected("Couldn't parse declaration",
		                     T_func, T_var, T_extern, T_struct, T_concept,
		                     T_instance, 0);
		eat_until_anchor();
		return;
	}

	parse();
}

static void register_declaration_parsers(void)
{
	register_declaration_parser(parse_function_declaration, T_func);
	register_declaration_parser(parse_global_variable,      T_var);
	register_declaration_parser(parse_constant,             T_const);
	register_declaration_parser(parse_struct,               T_struct);
	register_declaration_parser(parse_union,                T_union);
	register_declaration_parser(parse_typealias,            T_typealias);
	register_declaration_parser(parse_concept,              T_concept);
	register_declaration_parser(parse_concept_instance,     T_instance);
	register_declaration_parser(parse_export,               T_export);
	register_declaration_parser(parse_import,               T_import);
	register_declaration_parser(parse_module,               T_module);
	register_declaration_parser(skip_declaration,           T_NEWLINE);
}

static module_t *get_module(symbol_t *name)
{
	if (name == NULL) {
		name = symbol_table_insert("");
	}

	/* search for an existing module */
	module_t *module = modules;
	for ( ; module != NULL; module = module->next) {
		if (module->name == name)
			break;
	}
	if (module == NULL) {
		module       = allocate_ast_zero(sizeof(module[0]));
		module->name = name;
		module->next = modules;
		modules      = module;
	}
	return module;
}

static void append_context(context_t *dest, const context_t *source)
{
	entity_t *last = dest->entities;
	if (last != NULL) {
		while (last->base.next != NULL) {
			last = last->base.next;
		}
		last->base.next = source->entities;
	} else {
		dest->entities = source->entities;
	}

	concept_instance_t *last_concept_instance = dest->concept_instances;
	if (last_concept_instance != NULL) {
		while (last_concept_instance->next != NULL) {
			last_concept_instance = last_concept_instance->next;
		}
		last_concept_instance->next = source->concept_instances;
	} else {
		dest->concept_instances = source->concept_instances;
	}

	export_t *last_export = dest->exports;
	if (last_export != NULL) {
		while (last_export->next != NULL) {
			last_export = last_export->next;
		}
		last_export->next = source->exports;
	} else {
		dest->exports = source->exports;
	}

	import_t *last_import = dest->imports;
	if (last_import != NULL) {
		while (last_import->next != NULL) {
			last_import = last_import->next;
		}
		last_import->next = source->imports;
	} else {
		dest->imports = source->imports;
	}
}

bool parse_file(FILE *in, const char *input_name)
{
	memset(token_anchor_set, 0, sizeof(token_anchor_set));

	/* get the lexer running */
	input_t *input = input_from_stream(in, NULL);
	lexer_init(input, input_name);
	next_token();

	context_t file_context;
	memset(&file_context, 0, sizeof(file_context));

	assert(current_context == NULL);
	current_context = &file_context;

	current_module_name = NULL;

	add_anchor_token(T_EOF);
	while (token.type != T_EOF) {
		parse_declaration();
	}
	rem_anchor_token(T_EOF);

	assert(current_context == &file_context);
	current_context = NULL;

	/* append stuff to module */
	module_t *module = get_module(current_module_name);
	append_context(&module->context, &file_context);

	/* check that we have matching rem_anchor_token calls for each add */
#ifndef NDEBUG
	for (int i = 0; i < T_LAST_TOKEN; ++i) {
		if (token_anchor_set[i] > 0) {
			panic("leaked token");
		}
	}
#endif

	lexer_destroy();
	input_free(input);

	return !error;
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

