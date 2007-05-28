#include <config.h>

#include "type_t.h"
#include "type_hash.h"
#include "adt/error.h"

static struct obstack  _type_obst;
struct obstack        *type_obst = &_type_obst;

static type_t type_void_    = { TYPE_VOID, NULL };
static type_t type_invalid_ = { TYPE_INVALID, NULL };
type_t *type_void    = &type_void_;
type_t *type_invalid = &type_invalid_;

void init_type_module()
{
	obstack_init(type_obst);
}

void exit_type_module()
{
	obstack_free(type_obst, NULL);
}

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
	fprintf(out, "<?%s>", type->symbol->string);
}

static
void print_type_reference_variable(FILE *out, const type_reference_t *type)
{
	type_variable_t *type_variable = type->r.type_variable;

	fprintf(out, "%s:", type_variable->symbol->string);

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
}

void print_type(FILE *out, const type_t *type)
{
	if(type == NULL) {
		fputs("nil type", out);
		return;
	}

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
		fprintf(out, "%s", ((const struct_type_t*) type)->symbol->string);
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
	default:
		fputs("unknown", out);
		break;
	}
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

static
type_t *create_concrete_struct_type(struct_type_t *type)
{
	/* TODO: handle structs with typevars */
	return (type_t*) type;
}

static
type_t *create_concrete_method_type(method_type_t *type)
{
	int need_new_type = 0;

	method_type_t *new_type = obstack_alloc(type_obst, sizeof(new_type[0]));
	new_type->type.type     = TYPE_METHOD;
	
	type_t *result_type = create_concrete_type(type->result_type);
	if(result_type != type->result_type)
		need_new_type = 1;
	new_type->result_type = result_type;

	method_parameter_type_t *parameter_type      = type->parameter_types;
	method_parameter_type_t *last_parameter_type = NULL;
	while(parameter_type != NULL) {
		type_t *param_type     = parameter_type->type;
		type_t *new_param_type = create_concrete_type(param_type);

		if(new_param_type != param_type)
			need_new_type = 1;

		method_parameter_type_t *new_parameter_type
			= obstack_alloc(type_obst, sizeof(new_parameter_type[0]));
		memset(new_parameter_type, 0, sizeof(new_parameter_type[0]));
		new_parameter_type->type = new_param_type;

		if(last_parameter_type != NULL) {
			last_parameter_type->next = new_parameter_type;
		} else {
			new_type->parameter_types = new_parameter_type;
		}
		last_parameter_type = new_parameter_type;

		parameter_type = parameter_type->next;
	}

	if(!need_new_type) {
		obstack_free(type_obst, new_type);
		new_type = type;
	}

	return (type_t*) new_type;
}

static
type_t *create_concrete_pointer_type(pointer_type_t *type)
{
	type_t *points_to = create_concrete_type(type->points_to);

	if(points_to == type->points_to)
		return (type_t*) type;

	pointer_type_t *new_type = obstack_alloc(type_obst, sizeof(new_type[0]));
	memset(new_type, 0, sizeof(new_type[0]));
	new_type->type.type = TYPE_POINTER;
	new_type->points_to = points_to;

	type_t *normalized_type = typehash_insert((type_t*) new_type);
	if(normalized_type != (type_t*) new_type) {
		obstack_free(type_obst, new_type);
	}

	return normalized_type;
}

static
type_t *create_concrete_type_variable_reference_type(type_reference_t *type)
{
	type_variable_t *type_variable = type->r.type_variable;
	type_t          *current_type  = type_variable->current_type;

	if(current_type != NULL)
		return current_type;

	return (type_t*) type;
}

type_t *create_concrete_type(type_t *type)
{
	switch(type->type) {
	case TYPE_INVALID:
		return type_invalid;
	case TYPE_VOID:
		return type_void;
	case TYPE_ATOMIC:
		return type;
	case TYPE_STRUCT:
		return create_concrete_struct_type((struct_type_t*) type);
	case TYPE_METHOD:
		return create_concrete_method_type((method_type_t*) type);
	case TYPE_POINTER:
		return create_concrete_pointer_type((pointer_type_t*) type);
	case TYPE_REFERENCE_TYPE_VARIABLE:
		return create_concrete_type_variable_reference_type(
				(type_reference_t*) type);
	case TYPE_REFERENCE:
		panic("trying to normalize unresolved type reference");
		break;
	}

	return type;
}

