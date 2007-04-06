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
void print_ref_type(FILE *out, const ref_type_t *type)
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
		abort();
		break;
	case TYPE_METHOD:
		print_method_type(out, (const method_type_t*) type);
		break;
	case TYPE_POINTER:
		print_pointer_type(out, (const pointer_type_t*) type);
		break;
	case TYPE_REF:
		print_ref_type(out, (const ref_type_t*) type);
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

