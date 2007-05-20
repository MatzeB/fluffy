#include <config.h>

#include "ast_t.h"

#include <stdio.h>
#include <stdlib.h>

#include "adt/error.h"

static
void print_atomic_type(FILE *out, const atomic_type_t *type)
{
	switch(type->atype) {
	case ATOMIC_TYPE_INVALID:   fputs("INVALIDATOMIC", out); break;
	case ATOMIC_TYPE_BOOL:      fputs("bool", out); break;
	case ATOMIC_TYPE_BYTE:      fputs("byte", out); break;
	case ATOMIC_TYPE_UBYTE:     fputs("unsigned byte", out); break;
	case ATOMIC_TYPE_INT:       fputs("int", out); break;
	case ATOMIC_TYPE_UINT:      fputs("unsigned int", out); break;
	case ATOMIC_TYPE_SHORT:     fputs("short", out); break;
	case ATOMIC_TYPE_USHORT:    fputs("unsigned short", out); break;
	case ATOMIC_TYPE_LONG:      fputs("long", out); break;
	case ATOMIC_TYPE_ULONG:     fputs("unsigned long", out); break;
	case ATOMIC_TYPE_LONGLONG:  fputs("long long", out); break;
	case ATOMIC_TYPE_ULONGLONG: fputs("unsigned long long", out); break;
	case ATOMIC_TYPE_FLOAT:     fputs("float", out); break;
	case ATOMIC_TYPE_DOUBLE:    fputs("double", out); break;
	default:                    fputs("UNKNOWNATOMIC", out); break;
	}
}

static
void print_method_type(FILE *out, const method_type_t *type)
{
	fputs("<", out);
	print_type(out, type->result_type);
	fputs(" ", out);

	if(type->abi_style != NULL) {
		fprintf(out, "\"%s\" ", type->abi_style);
	}
	fputs("method(", out);
	method_parameter_type_t *param_type = type->parameter_types;
	int first = 1;
	while(param_type != NULL) {
		if(first) {
			first = 0;
		} else {
			fputs(", ", out);
		}
		print_type(out, param_type->type);
		param_type = param_type->next;
	}
	fputs(")>", out);
}

static
void print_pointer_type(FILE *out, const pointer_type_t *type)
{
	print_type(out, type->points_to);
	fputs("*", out);
}

static
void print_type_reference(FILE *out, const type_reference_t *type)
{
	fprintf(out, "<%s>", type->symbol->string);
}

static
void print_type_reference_variable(FILE *out, const type_reference_t *type)
{
	type_variable_t *type_variable = type->r.type_variable;

	fprintf(out, "<%s:", type_variable->symbol->string);

	type_constraint_t *constraint = type_variable->constraints;
	int first = 1;
	while(constraint != NULL) {
		if(first) {
			first = 0;
		} else {
			fprintf(out, ", ");
		}
		fprintf(out, "%s", constraint->typeclass_symbol->string);
		
		constraint = constraint->next;
	}
	fprintf(out, ">");
}

static
void print_type_reference_type(FILE *out, const type_reference_t *type)
{
	fprintf(out, "<%s>", type->symbol->string);
}

void print_type(FILE *out, const type_t *type)
{
	switch(type->type) {
	case TYPE_INVALID:
		fputs("invalid", out);
		break;
	case TYPE_VOID:
		fputs("void", out);
		break;
	case TYPE_ATOMIC:
		print_atomic_type(out, (const atomic_type_t*) type);
		break;
	case TYPE_STRUCT:
		fprintf(out, "<%s>", ((const struct_type_t*) type)->symbol->string);
		break;
	case TYPE_METHOD:
		print_method_type(out, (const method_type_t*) type);
		break;
	case TYPE_POINTER:
		print_pointer_type(out, (const pointer_type_t*) type);
		break;
	case TYPE_REFERENCE:
		print_type_reference(out, (const type_reference_t*) type);
		break;
	case TYPE_REFERENCE_TYPE_VARIABLE:
		print_type_reference_variable(out, (const type_reference_t*) type);
		break;
	case TYPE_REFERENCE_TYPE:
		print_type_reference_type(out, (const type_reference_t*) type);
		break;
	default:
		fputs("unknown", out);
		break;
	}
}

void print_expression(FILE *out, const expression_t *expression)
{
	/* TODO */
	fprintf(out, "some expression of type %d", expression->type);
}

int type_valid(const type_t *type)
{
	switch(type->type) {
	case TYPE_INVALID:
	case TYPE_REFERENCE:
		return 0;
	default:
		return 1;
	}
}

int is_type_int(const type_t *type)
{
	if(type->type != TYPE_ATOMIC)
		return 0;

	atomic_type_t *atomic_type = (atomic_type_t*) type;
	switch(atomic_type->atype) {
	case ATOMIC_TYPE_BYTE:
	case ATOMIC_TYPE_UBYTE:
	case ATOMIC_TYPE_SHORT:
	case ATOMIC_TYPE_USHORT:
	case ATOMIC_TYPE_INT:
	case ATOMIC_TYPE_UINT:
	case ATOMIC_TYPE_LONG:
	case ATOMIC_TYPE_ULONG:
	case ATOMIC_TYPE_LONGLONG:
	case ATOMIC_TYPE_ULONGLONG:
		return 1;
	default:
		return 0;
	}
}

static __attribute__((unused))
void dbg_type(const type_t *type)
{
	print_type(stdout,type);
	puts("\n");
	fflush(stdout);
}

static __attribute__((unused))
void dbg_pos(const source_position_t source_position)
{
	fprintf(stdout, "%s:%d\n", source_position.input_name, source_position.linenr);
	fflush(stdout);
}
