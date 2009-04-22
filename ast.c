#include <config.h>

#include "ast_t.h"
#include "type_t.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/error.h"

struct obstack  ast_obstack;
namespace_t    *namespaces;

static FILE *out;
static int   indent = 0;

static void print_expression(const expression_t *expression);
static void print_statement(const statement_t *statement);

static void print_int_const(const int_const_t *int_const)
{
	fprintf(out, "%d", int_const->value);
}

static void print_string_const(const string_const_t *string_const)
{
	/* TODO escape " and non-printable chars */
	fputc('"', out);
	for(const char *c = string_const->value; *c != 0; ++c) {
		switch(*c) {
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
	print_expression(call->method);
	fprintf(out, "(");
	call_argument_t *argument = call->arguments;
	int              first    = 1;
	while(argument != NULL) {
		if(!first) {
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

	while(argument != NULL) {
		if(first) {
			fprintf(out, "<$");
			first = 0;
		} else {
			fprintf(out, ", ");
		}
		print_type(argument->type);

		argument = argument->next;
	}
	if(type_arguments != NULL) {
		fprintf(out, ">");
	}
}

static void print_reference_expression(const reference_expression_t *ref)
{
	if(ref->declaration == NULL) {
		fprintf(out, "?%s", ref->symbol->string);
	} else {
		fprintf(out, "%s", ref->declaration->symbol->string);
	}

	print_type_arguments(ref->type_arguments);
}

static void print_select_expression(const select_expression_t *select)
{
	fprintf(out, "(");
	print_expression(select->compound);
	fprintf(out, ").");

	if(select->compound_entry != NULL) {
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
	switch(unexpr->type) {
	case UNEXPR_CAST:
		fprintf(out, "cast<");
		print_type(unexpr->expression.datatype);
		fprintf(out, "> ");
		print_expression(unexpr->value);
		break;
	default:
		fprintf(out, "*unexpr %d*", unexpr->type);
		break;
	}
	fprintf(out, ")");
}

static void print_binary_expression(const binary_expression_t *binexpr)
{
	fprintf(out, "(");
	print_expression(binexpr->left);
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
	case BINEXPR_MUL:
		fprintf(out, "*");
		break;
	case BINEXPR_DIV:
		fprintf(out, "/");
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
	print_expression(binexpr->right);
	fprintf(out, ")");
}

void print_expression(const expression_t *expression)
{
	if(expression == NULL) {
		fprintf(out, "*null expression*");
		return;
	}

	switch(expression->type) {
	case EXPR_LAST:
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
	case EXPR_BINARY:
		print_binary_expression((const binary_expression_t*) expression);
		break;
	case EXPR_UNARY:
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
	default:
		/* TODO */
		fprintf(out, "some expression of type %d", expression->type);
		break;
	}
}

static void print_indent(void)
{
	for(int i = 0; i < indent; ++i)
		fprintf(out, "\t");
}

static void print_block_statement(const block_statement_t *block)
{
	statement_t *statement = block->statements;
	while(statement != NULL) {
		indent++;
		print_statement(statement);
		indent--;

		statement = statement->next;
	}
}

static void print_return_statement(const return_statement_t *statement)
{
	fprintf(out, "return ");
	if(statement->return_value != NULL)
		print_expression(statement->return_value);
}

static void print_expression_statement(const expression_statement_t *statement)
{
	print_expression(statement->expression);
}

static void print_goto_statement(const goto_statement_t *statement)
{
	fprintf(out, "goto ");
	if(statement->label != NULL) {
		symbol_t *symbol = statement->label->declaration.symbol;
		if(symbol == NULL) {
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
	symbol_t *symbol = statement->declaration.declaration.symbol;
	if(symbol != NULL) {
		fprintf(out, ":%s", symbol->string);
	} else {
		const label_declaration_t *label = &statement->declaration;
		fprintf(out, ":$%p$", label);
	}
}

static void print_if_statement(const if_statement_t *statement)
{
	fprintf(out, "if ");
	print_expression(statement->condition);
	fprintf(out, ":\n");
	if(statement->true_statement != NULL)
		print_statement(statement->true_statement);

	if(statement->false_statement != NULL) {
		print_indent();
		fprintf(out, "else:\n");
		print_statement(statement->false_statement);
	}
}

static void print_variable_declaration(const variable_declaration_t *var)
{
	fprintf(out, "var");
	if(var->type != NULL) {
		fprintf(out, "<");
		print_type(var->type);
		fprintf(out, ">");
	}
	fprintf(out, " %s", var->declaration.symbol->string);
}

static void print_variable_declaration_statement(
		                     const variable_declaration_statement_t *statement)
{
	print_variable_declaration(&statement->declaration);
}

void print_statement(const statement_t *statement)
{
	print_indent();

	switch(statement->type) {
	case STATEMENT_BLOCK:
		print_block_statement((const block_statement_t*) statement);
		break;
	case STATEMENT_RETURN:
		print_return_statement((const return_statement_t*) statement);
		break;
	case STATEMENT_EXPRESSION:
		print_expression_statement((const expression_statement_t*) statement);
		break;
	case STATEMENT_LABEL:
		print_label_statement((const label_statement_t*) statement);
		break;
	case STATEMENT_GOTO:
		print_goto_statement((const goto_statement_t*) statement);
		break;
	case STATEMENT_IF:
		print_if_statement((const if_statement_t*) statement);
		break;
	case STATEMENT_VARIABLE_DECLARATION:
		print_variable_declaration_statement(
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

static void print_type_constraint(const type_constraint_t *constraint)
{
	if(constraint->concept == NULL) {
		fprintf(out, "?%s", constraint->concept_symbol->string);
	} else {
		fprintf(out, "%s", constraint->concept->declaration.symbol->string);
	}
}

static void print_type_variable(const type_variable_t *type_variable)
{
	type_constraint_t *constraint = type_variable->constraints;
	while(constraint != NULL) {
		print_type_constraint(constraint);
		fprintf(out, " ");

		constraint = constraint->next;
	}

	fprintf(out, "%s", type_variable->declaration.symbol->string);
}

static void print_type_parameters(const type_variable_t *type_parameters)
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
		print_type_variable(type_parameter);
		
		type_parameter = type_parameter->next;
	}
	if(type_parameters != NULL)
		fprintf(out, ">");
}

static void print_method_parameters(const method_parameter_t *parameters,
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

		print_type(parameter_type->type);
		fprintf(out, " %s", parameter->declaration.symbol->string);

		parameter      = parameter->next;
		parameter_type = parameter_type->next;
	}
	assert(parameter == NULL && parameter_type == NULL);

	fprintf(out, ")");
}

static void print_method(const method_declaration_t *method_declaration)
{
	const method_t *method = &method_declaration->method;
	method_type_t  *type   = method->type;

	fprintf(out, "func ");

	if(method->is_extern) {
		fprintf(out, "extern ");
	}

	fprintf(out, " %s", method_declaration->declaration.symbol->string);

	print_type_parameters(method->type_parameters);

	print_method_parameters(method->parameters, type);

	fprintf(out, " : ");
	print_type(type->result_type);

	if(method->statement != NULL) {
		fprintf(out, ":\n");
		print_statement(method->statement);
	} else {
		fprintf(out, "\n");
	}
}

static void print_concept_method(const concept_method_t *method)
{
	fprintf(out, "\tfunc ");
	fprintf(out, "%s", method->declaration.symbol->string);
	print_method_parameters(method->parameters, method->method_type);
	fprintf(out, " : ");
	print_type(method->method_type->result_type);
	fprintf(out, "\n\n");
}

static void print_concept(const concept_t *concept)
{
	fprintf(out, "concept %s", concept->declaration.symbol->string);
	print_type_parameters(concept->type_parameters);
	fprintf(out, ":\n");

	concept_method_t *method = concept->methods;
	while(method != NULL) {
		print_concept_method(method);

		method = method->next;
	}
}

static void print_concept_method_instance(
                                   concept_method_instance_t *method_instance)
{
	fprintf(out, "\tfunc ");

	const method_t *method = &method_instance->method;
	if(method_instance->concept_method != NULL) {
		concept_method_t *method = method_instance->concept_method;
		fprintf(out, "%s", method->declaration.symbol->string);
	} else {
		fprintf(out, "?%s", method_instance->symbol->string);
	}

	print_method_parameters(method->parameters, method->type);

	fprintf(out, " : ");
	print_type(method_instance->method.type->result_type);

	if(method->statement != NULL) {
		fprintf(out, ":\n");
		print_statement(method->statement);
	} else {
		fprintf(out, "\n");
	}
}

static void print_concept_instance(const concept_instance_t *instance)
{
	fprintf(out, "instance ");
	if(instance->concept != NULL) {
		fprintf(out, "%s", instance->concept->declaration.symbol->string);
	} else {
		fprintf(out, "?%s", instance->concept_symbol->string);
	}
	print_type_arguments(instance->type_arguments);
	fprintf(out, ":\n");

	concept_method_instance_t *method_instance = instance->method_instances;
	while(method_instance != NULL) {
		print_concept_method_instance(method_instance);

		method_instance = method_instance->next;
	}
}

static void print_constant(const constant_t *constant)
{
	fprintf(out, "const %s", constant->declaration.symbol->string);
	if(constant->type != NULL) {
		fprintf(out, " ");
		print_type(constant->type);
	}
	if(constant->expression != NULL) {
		fprintf(out, " <- ");
		print_expression(constant->expression);
	}
	fprintf(out, "\n");
}

static void print_typealias(const typealias_t *alias)
{
	fprintf(out, "typealias %s <- ", alias->declaration.symbol->string);
	print_type(alias->type);
	fprintf(out, "\n");
}

static void print_declaration(const declaration_t *declaration)
{
	print_indent();

	switch(declaration->type) {
	case DECLARATION_METHOD:
		print_method((const method_declaration_t*) declaration);
		break;
	case DECLARATION_CONCEPT:
		print_concept((const concept_t*) declaration);
		break;
	case DECLARATION_VARIABLE:
		print_variable_declaration((const variable_declaration_t*) declaration);
		break;
	case DECLARATION_TYPEALIAS:
		print_typealias((const typealias_t*) declaration);
		break;
	case DECLARATION_CONSTANT:
		print_constant((const constant_t*) declaration);
		break;
	case DECLARATION_ITERATOR:
	case DECLARATION_CONCEPT_METHOD:
	case DECLARATION_METHOD_PARAMETER:
		// TODO
		fprintf(out, "some declaration of type %d\n", declaration->type);
		break;

	case DECLARATION_TYPE_VARIABLE:
	case DECLARATION_LABEL:
		break;

	case DECLARATION_INVALID:
	case DECLARATION_LAST:
		fprintf(out, "invalid namespace declaration (%d)\n", declaration->type);
		break;
	}
}

static void print_context(const context_t *context)
{
	declaration_t *declaration = context->declarations;
	while(declaration != NULL) {
		print_declaration(declaration);
		declaration = declaration->next;
	}

	concept_instance_t *instance = context->concept_instances;
	while(instance != NULL) {
		print_concept_instance(instance);
		instance = instance->next;
	}
}

void print_ast(FILE *new_out, const namespace_t *namespace)
{
	indent = 0;
	out    = new_out;

	print_context(&namespace->context);

	assert(indent == 0);
	out = NULL;
}

const char *get_declaration_type_name(declaration_type_t type)
{
	switch(type) {
	case DECLARATION_LAST:
	case DECLARATION_INVALID:          return "invalid reference";
	case DECLARATION_VARIABLE:         return "variable";
	case DECLARATION_CONSTANT:         return "constant";
	case DECLARATION_METHOD_PARAMETER: return "method parameter";
	case DECLARATION_METHOD:           return "method";
	case DECLARATION_ITERATOR:         return "iterator";
	case DECLARATION_CONCEPT:          return "concept";
	case DECLARATION_TYPEALIAS:        return "type alias";
	case DECLARATION_TYPE_VARIABLE:    return "type variable";
	case DECLARATION_LABEL:            return "label";
	case DECLARATION_CONCEPT_METHOD:   return "concept method";
	}
	panic("invalid environment entry found");
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

unsigned register_expression()
{
	static unsigned nextid = EXPR_LAST;
	++nextid;
	return nextid;
}

unsigned register_statement()
{
	static unsigned nextid = STATEMENT_LAST;
	++nextid;
	return nextid;
}

unsigned register_declaration()
{
	static unsigned nextid = DECLARATION_LAST;
	++nextid;
	return nextid;
}

unsigned register_attribute()
{
	static unsigned nextid = 0;
	++nextid;
	return nextid;
}
