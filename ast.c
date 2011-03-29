#include <config.h>

#include "ast_t.h"
#include "type_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/error.h"

struct obstack  ast_obstack;

static FILE *out;
static int   indent = 0;

static void print_statement(const statement_t *statement);

static void print_int_const(const int_const_t *int_const)
{
	fprintf(out, "%d", int_const->value);
}

static void print_string_const(const string_const_t *string_const)
{
	/* TODO escape " and non-printable chars */
	fputc('"', out);
	for (const char *c = string_const->value; *c != 0; ++c) {
		switch (*c) {
		case '\a': fputs("\\a", out); break;
		case '\b': fputs("\\b", out); break;
		case '\f': fputs("\\f", out); break;
		case '\n': fputs("\\n", out); break;
		case '\r': fputs("\\r", out); break;
		case '\t': fputs("\\t", out); break;
		case '\v': fputs("\\v", out); break;
		case '\\': fputs("\\\\", out); break;
		case '"':  fputs("\\\"", out); break;
		default:   fputc(*c, out); break;
		}
	}
	fputc('"', out);
}

static void print_call_expression(const call_expression_t *call)
{
	print_expression(call->function);
	fprintf(out, "(");
	call_argument_t *argument = call->arguments;
	int              first    = 1;
	while (argument != NULL) {
		if (!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		print_expression(argument->expression);

		argument = argument->next;
	}
	fprintf(out, ")");
}

static void print_type_arguments(const type_argument_t *type_arguments)
{
	const type_argument_t *argument = type_arguments;
	int                    first    = 1;

	while (argument != NULL) {
		if (first) {
			fprintf(out, "<$");
			first = 0;
		} else {
			fprintf(out, ", ");
		}
		print_type(argument->type);

		argument = argument->next;
	}
	if (type_arguments != NULL) {
		fprintf(out, ">");
	}
}

static void print_reference_expression(const reference_expression_t *ref)
{
	if (ref->entity == NULL) {
		fprintf(out, "?%s", ref->symbol->string);
	} else {
		fprintf(out, "%s", ref->entity->base.symbol->string);
	}

	print_type_arguments(ref->type_arguments);
}

static void print_select_expression(const select_expression_t *select)
{
	fprintf(out, "(");
	print_expression(select->compound);
	fprintf(out, ").");

	if (select->compound_entry != NULL) {
		fputs(select->compound_entry->symbol->string, out);
	} else {
		fprintf(out, "?%s", select->symbol->string);
	}
}

static void print_array_access_expression(const array_access_expression_t *access)
{
	fprintf(out, "(");
	print_expression(access->array_ref);
	fprintf(out, ")[");
	print_expression(access->index);
	fprintf(out, "]");
}

static void print_sizeof_expression(const sizeof_expression_t *expr)
{
	fprintf(out, "(sizeof<");
	print_type(expr->type);
	fprintf(out, ">)");
}

static void print_unary_expression(const unary_expression_t *unexpr)
{
	fprintf(out, "(");
	switch (unexpr->base.kind) {
	case EXPR_UNARY_CAST:
		fprintf(out, "cast<");
		print_type(unexpr->base.type);
		fprintf(out, "> ");
		print_expression(unexpr->value);
		break;
	default:
		fprintf(out, "*unexpr %d*", unexpr->base.kind);
		break;
	}
	fprintf(out, ")");
}

static void print_binary_expression(const binary_expression_t *binexpr)
{
	fprintf(out, "(");
	print_expression(binexpr->left);
	fprintf(out, " ");
	switch (binexpr->base.kind) {
	case EXPR_BINARY_ASSIGN:
		fprintf(out, "<-");
		break;
	case EXPR_BINARY_ADD:
		fprintf(out, "+");
		break;
	case EXPR_BINARY_SUB:
		fprintf(out, "-");
		break;
	case EXPR_BINARY_MUL:
		fprintf(out, "*");
		break;
	case EXPR_BINARY_DIV:
		fprintf(out, "/");
		break;
	case EXPR_BINARY_NOTEQUAL:
		fprintf(out, "/=");
		break;
	case EXPR_BINARY_EQUAL:
		fprintf(out, "=");
		break;
	case EXPR_BINARY_LESS:
		fprintf(out, "<");
		break;
	case EXPR_BINARY_LESSEQUAL:
		fprintf(out, "<=");
		break;
	case EXPR_BINARY_GREATER:
		fprintf(out, ">");
		break;
	case EXPR_BINARY_GREATEREQUAL:
		fprintf(out, ">=");
		break;
	default:
		/* TODO: add missing ops */
		fprintf(out, "op%d", binexpr->base.kind);
		break;
	}
	fprintf(out, " ");
	print_expression(binexpr->right);
	fprintf(out, ")");
}

void print_expression(const expression_t *expression)
{
	if (expression == NULL) {
		fprintf(out, "*null expression*");
		return;
	}

	switch (expression->kind) {
	case EXPR_ERROR:
		fprintf(out, "*error expression*");
		break;
	case EXPR_INVALID:
		fprintf(out, "*invalid expression*");
		break;
	case EXPR_INT_CONST:
		print_int_const((const int_const_t*) expression);
		break;
	case EXPR_STRING_CONST:
		print_string_const((const string_const_t*) expression);
		break;
	case EXPR_NULL_POINTER:
		fprintf(out, "null");
		break;
	case EXPR_CALL:
		print_call_expression((const call_expression_t*) expression);
		break;
	EXPR_BINARY_CASES
		print_binary_expression((const binary_expression_t*) expression);
		break;
	EXPR_UNARY_CASES
		print_unary_expression((const unary_expression_t*) expression);
		break;
	case EXPR_SELECT:
		print_select_expression((const select_expression_t*) expression);
		break;
	case EXPR_ARRAY_ACCESS:
		print_array_access_expression(
				(const array_access_expression_t*) expression);
		break;
	case EXPR_SIZEOF:
		print_sizeof_expression((const sizeof_expression_t*) expression);
		break;
	case EXPR_REFERENCE:
		print_reference_expression((const reference_expression_t*) expression);
		break;
	case EXPR_FLOAT_CONST:
	case EXPR_BOOL_CONST:
	case EXPR_FUNC:
		/* TODO */
		fprintf(out, "*expr TODO*");
		break;
	}
}

static void print_indent(void)
{
	for (int i = 0; i < indent; ++i)
		fprintf(out, "\t");
}

static void print_block_statement(const block_statement_t *block)
{
	statement_t *statement = block->statements;
	for ( ; statement != NULL; statement = statement->base.next) {
		indent++;
		print_statement(statement);
		indent--;
	}
}

static void print_return_statement(const return_statement_t *statement)
{
	fprintf(out, "return ");
	if (statement->value != NULL)
		print_expression(statement->value);
}

static void print_expression_statement(const expression_statement_t *statement)
{
	print_expression(statement->expression);
}

static void print_goto_statement(const goto_statement_t *statement)
{
	fprintf(out, "goto ");
	if (statement->label != NULL) {
		symbol_t *symbol = statement->label->base.symbol;
		if (symbol == NULL) {
			fprintf(out, "$%p$", statement->label);
		} else {
			fprintf(out, "%s", symbol->string);
		}
	} else {
		fprintf(out, "?%s", statement->label_symbol->string);
	}
}

static void print_label_statement(const label_statement_t *statement)
{
	symbol_t *symbol = statement->label.base.symbol;
	if (symbol != NULL) {
		fprintf(out, ":%s", symbol->string);
	} else {
		const label_t *label = &statement->label;
		fprintf(out, ":$%p$", label);
	}
}

static void print_if_statement(const if_statement_t *statement)
{
	fprintf(out, "if ");
	print_expression(statement->condition);
	fprintf(out, ":\n");
	if (statement->true_statement != NULL)
		print_statement(statement->true_statement);

	if (statement->false_statement != NULL) {
		print_indent();
		fprintf(out, "else:\n");
		print_statement(statement->false_statement);
	}
}

static void print_variable(const variable_t *variable)
{
	fprintf(out, "var");
	if (variable->type != NULL) {
		fprintf(out, "<");
		print_type(variable->type);
		fprintf(out, ">");
	}
	fprintf(out, " %s", variable->base.symbol->string);
}

static void print_declaration_statement(const declaration_statement_t *statement)
{
	print_variable(&statement->entity);
}

void print_statement(const statement_t *statement)
{
	print_indent();

	switch (statement->kind) {
	case STATEMENT_BLOCK:
		print_block_statement(&statement->block);
		break;
	case STATEMENT_RETURN:
		print_return_statement(&statement->returns);
		break;
	case STATEMENT_EXPRESSION:
		print_expression_statement(&statement->expression);
		break;
	case STATEMENT_LABEL:
		print_label_statement(&statement->label);
		break;
	case STATEMENT_GOTO:
		print_goto_statement(&statement->gotos);
		break;
	case STATEMENT_IF:
		print_if_statement(&statement->ifs);
		break;
	case STATEMENT_DECLARATION:
		print_declaration_statement(&statement->declaration);
		break;
	case STATEMENT_INVALID:
	default:
		fprintf(out, "*invalid statement*");
		break;

	}
	fprintf(out, "\n");
}

static void print_type_constraint(const type_constraint_t *constraint)
{
	if (constraint->concept == NULL) {
		fprintf(out, "?%s", constraint->concept_symbol->string);
	} else {
		fprintf(out, "%s", constraint->concept->base.symbol->string);
	}
}

static void print_type_variable(const type_variable_t *type_variable)
{
	type_constraint_t *constraint = type_variable->constraints;
	while (constraint != NULL) {
		print_type_constraint(constraint);
		fprintf(out, " ");

		constraint = constraint->next;
	}

	fprintf(out, "%s", type_variable->base.symbol->string);
}

static void print_type_parameters(const type_variable_t *type_parameters)
{
	int                    first          = 1;
	const type_variable_t *type_parameter = type_parameters;
	while (type_parameter != NULL) {
		if (first) {
			fprintf(out, "<");
			first = 0;
		} else {
			fprintf(out, ", ");
		}
		print_type_variable(type_parameter);
		
		type_parameter = type_parameter->next;
	}
	if (type_parameters != NULL)
		fprintf(out, ">");
}

static void print_function_parameters(const function_parameter_t *parameters,
                                      const function_type_t    *function_type)
{
	fprintf(out, "(");

	int                              first          = 1;
	const function_parameter_t      *parameter      = parameters;
	const function_parameter_type_t *parameter_type 
		= function_type->parameter_types;
	while (parameter != NULL && parameter_type != NULL) {
		if (!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}

		print_type(parameter_type->type);
		fprintf(out, " %s", parameter->base.symbol->string);

		parameter      = parameter->next;
		parameter_type = parameter_type->next;
	}
	assert(parameter == NULL && parameter_type == NULL);

	fprintf(out, ")");
}

static void print_function(const function_entity_t *function_entity)
{
	const function_t *function = &function_entity->function;
	function_type_t  *type     = function->type;

	fprintf(out, "func ");

	if (function->is_extern) {
		fprintf(out, "extern ");
	}

	fprintf(out, " %s", function_entity->base.symbol->string);

	print_type_parameters(function->type_parameters);

	print_function_parameters(function->parameters, type);

	fprintf(out, " : ");
	print_type(type->result_type);

	if (function->statement != NULL) {
		fprintf(out, ":\n");
		print_statement(function->statement);
	} else {
		fprintf(out, "\n");
	}
}

static void print_concept_function(const concept_function_t *function)
{
	fprintf(out, "\tfunc ");
	fprintf(out, "%s", function->base.symbol->string);
	print_function_parameters(function->parameters, function->type);
	fprintf(out, " : ");
	print_type(function->type->result_type);
	fprintf(out, "\n\n");
}

static void print_concept(const concept_t *concept)
{
	fprintf(out, "concept %s", concept->base.symbol->string);
	print_type_parameters(concept->type_parameters);
	fprintf(out, ":\n");

	concept_function_t *function = concept->functions;
	while (function != NULL) {
		print_concept_function(function);

		function = function->next;
	}
}

static void print_concept_function_instance(
		concept_function_instance_t *function_instance)
{
	fprintf(out, "\tfunc ");

	const function_t *function = &function_instance->function;
	if (function_instance->concept_function != NULL) {
		concept_function_t *function = function_instance->concept_function;
		fprintf(out, "%s", function->base.symbol->string);
	} else {
		fprintf(out, "?%s", function_instance->symbol->string);
	}

	print_function_parameters(function->parameters, function->type);

	fprintf(out, " : ");
	print_type(function_instance->function.type->result_type);

	if (function->statement != NULL) {
		fprintf(out, ":\n");
		print_statement(function->statement);
	} else {
		fprintf(out, "\n");
	}
}

static void print_concept_instance(const concept_instance_t *instance)
{
	fprintf(out, "instance ");
	if (instance->concept != NULL) {
		fprintf(out, "%s", instance->concept->base.symbol->string);
	} else {
		fprintf(out, "?%s", instance->concept_symbol->string);
	}
	print_type_arguments(instance->type_arguments);
	fprintf(out, ":\n");

	concept_function_instance_t *function_instance
		= instance->function_instances;
	while (function_instance != NULL) {
		print_concept_function_instance(function_instance);

		function_instance = function_instance->next;
	}
}

static void print_constant(const constant_t *constant)
{
	fprintf(out, "const %s", constant->base.symbol->string);
	if (constant->type != NULL) {
		fprintf(out, " ");
		print_type(constant->type);
	}
	if (constant->expression != NULL) {
		fprintf(out, " <- ");
		print_expression(constant->expression);
	}
	fprintf(out, "\n");
}

static void print_typealias(const typealias_t *alias)
{
	fprintf(out, "typealias %s <- ", alias->base.symbol->string);
	print_type(alias->type);
	fprintf(out, "\n");
}

static void print_entity(const entity_t *entity)
{
	print_indent();

	switch (entity->kind) {
	case ENTITY_FUNCTION:
		print_function(&entity->function);
		break;
	case ENTITY_CONCEPT:
		print_concept(&entity->concept);
		break;
	case ENTITY_VARIABLE:
		print_variable(&entity->variable);
		break;
	case ENTITY_TYPEALIAS:
		print_typealias(&entity->typealias);
		break;
	case ENTITY_CONSTANT:
		print_constant(&entity->constant);
		break;
	case ENTITY_CONCEPT_FUNCTION:
	case ENTITY_FUNCTION_PARAMETER:
	case ENTITY_ERROR:
		// TODO
		fprintf(out, "some entity of type '%s'\n",
		        get_entity_kind_name(entity->kind));
		break;

	case ENTITY_TYPE_VARIABLE:
	case ENTITY_LABEL:
		break;

	case ENTITY_INVALID:
		fprintf(out, "invalid entity (%s)\n",
		        get_entity_kind_name(entity->kind));
		break;
	}
}

static void print_context(const context_t *context)
{
	for (entity_t *entity = context->entities; entity != NULL;
	     entity = entity->base.next) {
		print_entity(entity);
	}

	concept_instance_t *instance = context->concept_instances;
	for ( ; instance != NULL; instance = instance->next) {
		print_concept_instance(instance);
	}
}

void print_ast(FILE *new_out, const context_t *context)
{
	indent = 0;
	out    = new_out;

	print_context(context);

	assert(indent == 0);
	out = NULL;
}

const char *get_entity_kind_name(entity_kind_t type)
{
	switch (type) {
	case ENTITY_ERROR:              return "parse error";
	case ENTITY_INVALID:            return "invalid reference";
	case ENTITY_VARIABLE:           return "variable";
	case ENTITY_CONSTANT:           return "constant";
	case ENTITY_FUNCTION_PARAMETER: return "function parameter";
	case ENTITY_FUNCTION:           return "function";
	case ENTITY_CONCEPT:            return "concept";
	case ENTITY_TYPEALIAS:          return "type alias";
	case ENTITY_TYPE_VARIABLE:      return "type variable";
	case ENTITY_LABEL:              return "label";
	case ENTITY_CONCEPT_FUNCTION:   return "concept function";
	}
	panic("invalid environment entry found");
}

void init_ast_module(void)
{
	out = stderr;
	obstack_init(&ast_obstack);
}

void exit_ast_module(void)
{
	obstack_free(&ast_obstack, NULL);
}

void* (allocate_ast) (size_t size)
{
	return _allocate_ast(size);
}

unsigned register_expression(void)
{
	static unsigned nextid = EXPR_LAST;
	++nextid;
	return nextid;
}

unsigned register_statement(void)
{
	static unsigned nextid = STATEMENT_LAST;
	++nextid;
	return nextid;
}

unsigned register_entity(void)
{
	static unsigned nextid = ENTITY_LAST;
	++nextid;
	return nextid;
}

bool is_linktime_constant(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_SELECT:
		/* TODO */
		return false;
	case EXPR_ARRAY_ACCESS:
		/* TODO */
		return false;
	case EXPR_UNARY_DEREFERENCE:
		return is_constant_expression(expression->unary.value);
	default:
		return false;
	}
}

bool is_constant_expression(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_INT_CONST:
	case EXPR_FLOAT_CONST:
	case EXPR_BOOL_CONST:
	case EXPR_NULL_POINTER:
	case EXPR_SIZEOF:
		return true;

	case EXPR_STRING_CONST:
	case EXPR_FUNC:
	case EXPR_UNARY_INCREMENT:
	case EXPR_UNARY_DECREMENT:
	case EXPR_UNARY_DEREFERENCE:
	case EXPR_BINARY_ASSIGN:
	case EXPR_SELECT:
	case EXPR_ARRAY_ACCESS:
		return false;

	case EXPR_UNARY_TAKE_ADDRESS:
		return is_linktime_constant(expression->unary.value);

	case EXPR_REFERENCE: {
		entity_t *entity = expression->reference.entity;
		if (entity->kind == ENTITY_CONSTANT)
			return true;
		return false;
	}

	case EXPR_CALL:
		/* TODO: we might introduce pure/side effect free calls */
		return false;

	case EXPR_UNARY_CAST:
	case EXPR_UNARY_NEGATE:
	case EXPR_UNARY_NOT:
	case EXPR_UNARY_BITWISE_NOT:
		return is_constant_expression(expression->unary.value);

	case EXPR_BINARY_ADD:
	case EXPR_BINARY_SUB:
	case EXPR_BINARY_MUL:
	case EXPR_BINARY_DIV:
	case EXPR_BINARY_MOD:
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
	case EXPR_BINARY_AND:
	case EXPR_BINARY_OR:
	case EXPR_BINARY_XOR:
	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTRIGHT:
	/* not that lazy and/or are not constant if their value is clear after
	 * evaluating the left side. This is because we can't (always) evaluate the
	 * left hand side until the ast2firm phase, and therefore can't determine
	 * constness */
	case EXPR_BINARY_LAZY_AND:
	case EXPR_BINARY_LAZY_OR:
		return is_constant_expression(expression->binary.left)
			&& is_constant_expression(expression->binary.right);

	case EXPR_ERROR:
		return true;
	case EXPR_INVALID:
		break;
	}
	panic("invalid expression in is_constant_expression");
}

