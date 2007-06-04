#include <config.h>

#include "ast_t.h"
#include "type_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/error.h"

struct obstack ast_obstack;

static
void print_int_const(FILE *out, const int_const_t *int_const)
{
	fprintf(out, "%d", int_const->value);
}

static
void print_string_const(FILE *out, const string_const_t *string_const)
{
	/* TODO escape " and non-printable chars */
	fprintf(out, "\"%s\"", string_const->value);
}

static
void print_call_expression(FILE *out, const call_expression_t *call)
{
	print_expression(out, call->method);
	fprintf(out, "(");
	call_argument_t *argument = call->arguments;
	int              first    = 1;
	while(argument != NULL) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}
		print_expression(out, argument->expression);

		argument = argument->next;
	}
	fprintf(out, ")");
}

static
void print_type_arguments(FILE *out, const type_argument_t *type_arguments)
{
	const type_argument_t *argument = type_arguments;
	int                    first    = 1;

	while(argument != NULL) {
		if(first) {
			fprintf(out, "<$");
			first = 0;
		} else {
			fprintf(out, ", ");
		}
		print_type(out, argument->type);

		argument = argument->next;
	}
	if(type_arguments != NULL) {
		fprintf(out, ">");
	}
}

static
void print_reference_expression(FILE *out, const reference_expression_t *ref)
{
	if(ref->expression.type == EXPR_REFERENCE) {
		fprintf(out, "?");
	}
	fprintf(out, "%s", ref->symbol->string);

	print_type_arguments(out, ref->type_arguments);
}

static
void print_binary_expression(FILE *out, const binary_expression_t *binexpr)
{
	fprintf(out, "(");
	print_expression(out, binexpr->left);
	fprintf(out, " ");
	switch(binexpr->type) {
	case BINEXPR_INVALID:
		fprintf(out, "INVOP");
		break;
	case BINEXPR_ASSIGN:
		fprintf(out, "<-");
		break;
	case BINEXPR_ADD:
		fprintf(out, "+");
		break;
	case BINEXPR_SUB:
		fprintf(out, "-");
		break;
	case BINEXPR_NOTEQUAL:
		fprintf(out, "/=");
		break;
	case BINEXPR_EQUAL:
		fprintf(out, "=");
		break;
	case BINEXPR_LESS:
		fprintf(out, "<");
		break;
	case BINEXPR_LESSEQUAL:
		fprintf(out, "<=");
		break;
	case BINEXPR_GREATER:
		fprintf(out, ">");
		break;
	case BINEXPR_GREATEREQUAL:
		fprintf(out, ">=");
		break;
	default:
		/* TODO: add missing ops */
		fprintf(out, "op%d", binexpr->type);
		break;
	}
	fprintf(out, " ");
	print_expression(out, binexpr->right);
	fprintf(out, ")");
}

void print_expression(FILE *out, const expression_t *expression)
{
	switch(expression->type) {
	case EXPR_LAST:
	case EXPR_INVALID:
		fprintf(out, "*invalid expression*");
		break;
	case EXPR_INT_CONST:
		print_int_const(out, (const int_const_t*) expression);
		break;
	case EXPR_STRING_CONST:
		print_string_const(out, (const string_const_t*) expression);
		break;
	case EXPR_CALL:
		print_call_expression(out, (const call_expression_t*) expression);
		break;
	case EXPR_BINARY:
		print_binary_expression(out, (const binary_expression_t*) expression);
		break;
	case EXPR_REFERENCE:
	case EXPR_REFERENCE_VARIABLE:
	case EXPR_REFERENCE_METHOD:
	case EXPR_REFERENCE_METHOD_PARAMETER:
	case EXPR_REFERENCE_GLOBAL_VARIABLE:
	case EXPR_REFERENCE_TYPECLASS_METHOD:
	case EXPR_REFERENCE_TYPECLASS_METHOD_INSTANCE:
		print_reference_expression(out,
		                           (const reference_expression_t*) expression);
		break;
	case EXPR_UNARY:
	case EXPR_SELECT:
	case EXPR_ARRAY_ACCESS:
	case EXPR_SIZEOF:
		/* TODO */
		fprintf(out, "some expression of type %d", expression->type);
		break;
	}
}

static
void print_block_statement(FILE *out, int indent,
                           const block_statement_t *block)
{
	statement_t *statement = block->first_statement;
	while(statement != NULL) {
		print_statement(out, indent + 1, statement);

		statement = statement->next;
	}
}

static
void print_return_statement(FILE *out, const return_statement_t *statement)
{
	fprintf(out, "return ");
	if(statement->return_value != NULL)
		print_expression(out, statement->return_value);
}

static
void print_expression_statement(FILE *out,
                                const expression_statement_t *statement)
{
	print_expression(out, statement->expression);
}

static
void print_goto_statement(FILE *out, const goto_statement_t *statement)
{
	fprintf(out, "goto ");
	if(statement->label != NULL) {
		fprintf(out, "%s", statement->label->symbol->string);
	} else {
		fprintf(out, "?%s", statement->label_symbol->string);
	}
}

static
void print_label_statement(FILE *out, const label_statement_t *statement)
{
	fprintf(out, ":%s", statement->symbol->string);
}

static
void print_if_statement(FILE *out, int indent, const if_statement_t *statement)
{
	fprintf(out, "if ");
	print_expression(out, statement->condition);
	fprintf(out, ":\n");
	if(statement->true_statement != NULL)
		print_statement(out, indent, statement->true_statement);

	if(statement->false_statement != NULL) {
		fprintf(out, "else:\n");
		print_statement(out, indent, statement->false_statement);
	}
}

static
void print_variable_declaration_statement(FILE *out,
                     const variable_declaration_statement_t *statement)
{
	fprintf(out, "var");
	if(statement->type != NULL) {
		fprintf(out, "<");
		print_type(out, statement->type);
		fprintf(out, ">");
	}
	fprintf(out, " %s", statement->symbol->string);
}

void print_statement(FILE *out, int indent, const statement_t *statement)
{
	for(int i = 0; i < indent; ++i)
		fprintf(out, "\t");

	switch(statement->type) {
	case STATEMENT_BLOCK:
		print_block_statement(out, indent,
		                      (const block_statement_t*) statement);
		break;
	case STATEMENT_RETURN:
		print_return_statement(out, (const return_statement_t*) statement);
		break;
	case STATEMENT_EXPRESSION:
		print_expression_statement(out,
		                           (const expression_statement_t*) statement);
		break;
	case STATEMENT_LABEL:
		print_label_statement(out, (const label_statement_t*) statement);
		break;
	case STATEMENT_GOTO:
		print_goto_statement(out, (const goto_statement_t*) statement);
		break;
	case STATEMENT_IF:
		print_if_statement(out, indent, (const if_statement_t*) statement);
		break;
	case STATEMENT_VARIABLE_DECLARATION:
		print_variable_declaration_statement(out,
		        (const variable_declaration_statement_t*) statement);
		break;
	case STATEMENT_LAST:
	case STATEMENT_INVALID:
	default:
		fprintf(out, "*invalid statement*");
		break;

	}
	fprintf(out, "\n");
}

static
void print_type_constraint(FILE *out, const type_constraint_t *constraint)
{
	if(constraint->typeclass == NULL) {
		fprintf(out, "?%s", constraint->typeclass_symbol->string);
	} else {
		fprintf(out, "%s", constraint->typeclass->symbol->string);
	}
}

static
void print_type_variable(FILE *out, const type_variable_t *type_variable)
{
	type_constraint_t *constraint = type_variable->constraints;
	while(constraint != NULL) {
		print_type_constraint(out, constraint);
		fprintf(out, " ");

		constraint = constraint->next;
	}

	fprintf(out, "%s", type_variable->symbol->string);
}

static
void print_type_parameters(FILE *out, const type_variable_t *type_parameters)
{
	int                    first          = 1;
	const type_variable_t *type_parameter = type_parameters;
	while(type_parameter != NULL) {
		if(first) {
			fprintf(out, "<");
			first = 0;
		} else {
			fprintf(out, ", ");
		}
		print_type_variable(out, type_parameter);
		
		type_parameter = type_parameter->next;
	}
	if(type_parameters != NULL)
		fprintf(out, ">");
}

static
void print_method_parameters(FILE *out, const method_parameter_t *parameters,
                             const method_type_t *method_type)
{
	fprintf(out, "(");

	int                            first          = 1;
	const method_parameter_t      *parameter      = parameters;
	const method_parameter_type_t *parameter_type 
		= method_type->parameter_types;
	while(parameter != NULL && parameter_type != NULL) {
		if(!first) {
			fprintf(out, ", ");
		} else {
			first = 0;
		}

		print_type(out, parameter_type->type);
		fprintf(out, " %s", parameter->symbol->string);

		parameter      = parameter->next;
		parameter_type = parameter_type->next;
	}
	assert(parameter == NULL && parameter_type == NULL);

	fprintf(out, ")");
}

static
void print_method(FILE *out, const method_t *method)
{
	method_type_t *type = method->type;

	if(method->is_extern) {
		fprintf(out, "extern ");
	}

	fprintf(out, "func ");
	print_type(out, type->result_type);
	fprintf(out, " %s", method->symbol->string);

	print_type_parameters(out, method->type_parameters);

	print_method_parameters(out, method->parameters, type);

	if(method->statement != NULL) {
		fprintf(out, ":\n");
		print_statement(out, 0, method->statement);
	} else {
		fprintf(out, "\n");
	}
}

static
void print_typeclass_method(FILE *out, const typeclass_method_t *method)
{
	fprintf(out, "\tfunc ");
	print_type(out, method->method_type->result_type);
	fprintf(out, "%s", method->symbol->string);
	print_method_parameters(out, method->parameters, method->method_type);
	fprintf(out, "\n\n");
}

static
void print_typeclass(FILE *out, const typeclass_t *typeclass)
{
	fprintf(out, "typeclass %s", typeclass->symbol->string);
	print_type_parameters(out, typeclass->type_parameters);
	fprintf(out, ":\n");

	typeclass_method_t *method = typeclass->methods;
	while(method != NULL) {
		print_typeclass_method(out, method);

		method = method->next;
	}
}

static
void print_typeclass_method_instance(FILE *out,
                                   typeclass_method_instance_t *method_instance)
{
	fprintf(out, "\tfunc ");
	print_type(out, method_instance->method->type->result_type);
	fprintf(out, " ");

	if(method_instance->typeclass_method != NULL) {
		typeclass_method_t *method = method_instance->typeclass_method;
		fprintf(out, "%s", method->symbol->string);
	} else {
		fprintf(out, "?%s", method_instance->method->symbol->string);
	}

	method_t *method = method_instance->method;
	print_method_parameters(out, method->parameters, method->type);

	if(method->statement != NULL) {
		fprintf(out, ":\n");
		print_statement(out, 1, method->statement);
	} else {
		fprintf(out, "\n");
	}
}

static
void print_typeclass_instance(FILE *out, const typeclass_instance_t *instance)
{
	fprintf(out, "instance ");
	if(instance->typeclass != NULL) {
		fprintf(out, "%s", instance->typeclass->symbol->string);
	} else {
		fprintf(out, "?%s", instance->typeclass_symbol->string);
	}
	print_type_arguments(out, instance->type_arguments);
	fprintf(out, ":\n");

	typeclass_method_instance_t *method_instance = instance->method_instances;
	while(method_instance != NULL) {
		print_typeclass_method_instance(out, method_instance);

		method_instance = method_instance->next;
	}
}

static
void print_namespace_entry(FILE *out, const namespace_entry_t *entry)
{
	switch(entry->type) {
	case NAMESPACE_ENTRY_METHOD:
		print_method(out, (const method_t*) entry);
		break;
	case NAMESPACE_ENTRY_TYPECLASS:
		print_typeclass(out, (const typeclass_t*) entry);
		break;
	case NAMESPACE_ENTRY_TYPECLASS_INSTANCE:
		print_typeclass_instance(out, (const typeclass_instance_t*) entry);
		break;
	case NAMESPACE_ENTRY_VARIABLE:
	case NAMESPACE_ENTRY_TYPEALIAS:
		/* TODO */
		fprintf(out, "some namespace entry of type %d\n\n", entry->type);
		break;
	case NAMESPACE_ENTRY_INVALID:
	case NAMESPACE_ENTRY_LAST:
	default:
		fprintf(out, "invalid namespace entry (%d)\n", entry->type);
		break;
	}
}

void print_ast(FILE *out, const namespace_t *namespace)
{
	namespace_entry_t *entry = namespace->entries;

	while(entry != NULL) {
		print_namespace_entry(out, entry);

		entry = entry->next;
	}
}

void init_ast_module(void)
{
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

int register_expression()
{
	static int nextid = EXPR_LAST;
	++nextid;
	return nextid;
}

int register_statement()
{
	static int nextid = STATEMENT_LAST;
	++nextid;
	return nextid;
}

int register_namespace_entry()
{
	static int nextid = NAMESPACE_ENTRY_LAST;
	++nextid;
	return nextid;
}

