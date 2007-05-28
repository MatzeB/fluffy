#include <config.h>

#include "ast_t.h"
#include "type_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/error.h"

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
void print_reference_expression(FILE *out, const reference_expression_t *ref)
{
	if(ref->expression.type == EXPR_REFERENCE) {
		fprintf(out, "?");
	}
	fprintf(out, "%s", ref->symbol->string);

	type_argument_t *type_argument = ref->type_arguments;
	int              first         = 1;
	while(type_argument != NULL) {
		if(first) {
			fprintf(out, "<");
			first = 0;
		} else {
			fprintf(out, ", ");
		}

		type_t *type = type_argument->type;
		print_type(out, type);

		type_argument = type_argument->next;
	}

	if(ref->type_arguments != NULL)
		fprintf(out, ">");
}

void print_expression(FILE *out, const expression_t *expression)
{
	switch(expression->type) {
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
	case EXPR_REFERENCE:
	case EXPR_REFERENCE_VARIABLE:
	case EXPR_REFERENCE_METHOD:
	case EXPR_REFERENCE_METHOD_PARAMETER:
	case EXPR_REFERENCE_EXTERN_METHOD:
	case EXPR_REFERENCE_GLOBAL_VARIABLE:
	case EXPR_REFERENCE_TYPECLASS_METHOD:
	case EXPR_REFERENCE_TYPECLASS_METHOD_INSTANCE:
		print_reference_expression(out,
		                           (const reference_expression_t*) expression);
		break;
	case EXPR_UNARY:
	case EXPR_BINARY:
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

void print_statement(FILE *out, int indent, const statement_t *statement)
{
	for(int i = 0; i < indent; ++i)
		fprintf(out, "\t");

	switch(statement->type) {
	case STATEMENT_INVALID:
		fprintf(out, "*invalid statement*");
		break;
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
	case STATEMENT_VARIABLE_DECLARATION:
	case STATEMENT_IF:
	case STATEMENT_GOTO:
	case STATEMENT_LABEL:
		/* TODO */
		fprintf(out, "some statement of type %d", statement->type);
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
void print_method(FILE *out, const method_t *method)
{
	method_type_t *type = method->type;

	fprintf(out, "func ");
	print_type(out, type->result_type);
	fprintf(out, " %s", method->symbol->string);

	int              first = 1;
	type_variable_t *type_parameter = method->type_parameters;
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
	if(method->type_parameters != NULL)
		fprintf(out, ">");
	fprintf(out, "(");

	first = 1;
	method_parameter_t      *parameter      = method->parameters;
	method_parameter_type_t *parameter_type = type->parameter_types;
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

	fprintf(out, "):\n");
	print_statement(out, 0, method->statement);
}

static
void print_namespace_entry(FILE *out, const namespace_entry_t *entry)
{
	switch(entry->type) {
	case NAMESPACE_ENTRY_METHOD:
		print_method(out, (const method_t*) entry);
		break;
	case NAMESPACE_ENTRY_VARIABLE:
	case NAMESPACE_ENTRY_EXTERN_METHOD:
	case NAMESPACE_ENTRY_STRUCT:
	case NAMESPACE_ENTRY_TYPECLASS:
	case NAMESPACE_ENTRY_TYPECLASS_INSTANCE:
		/* TODO */
		fprintf(out, "some namespace entry of type %d\n\n", entry->type);
		break;
	}
}

void print_ast(FILE *out, const namespace_t *namespace)
{
	namespace_entry_t *entry = namespace->first_entry;

	while(entry != NULL) {
		print_namespace_entry(out, entry);

		entry = entry->next;
	}
}

