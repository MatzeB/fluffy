#include <config.h>

#include "mangle_type.h"
#include "type_t.h"
#include "adt/error.h"

static
void mangle_atomic_type(struct obstack *obst, const atomic_type_t *type)
{
	char c;

	switch(type->atype) {
	case ATOMIC_TYPE_INVALID:
		abort();
		break;
	case ATOMIC_TYPE_BOOL:
		c = 'b';
		break;
	case ATOMIC_TYPE_BYTE:
		c = 'c';
		break;
	case ATOMIC_TYPE_UBYTE:
		c = 'h';
		break;
	case ATOMIC_TYPE_INT:
		c = 'i';
		break;
	case ATOMIC_TYPE_UINT:
		c = 'j';
		break;
	case ATOMIC_TYPE_SHORT:
		c = 's';
		break;
	case ATOMIC_TYPE_USHORT:
		c = 't';
		break;
	case ATOMIC_TYPE_LONG:
		c = 'l';
		break;
	case ATOMIC_TYPE_ULONG:
		c = 'm';
		break;
	case ATOMIC_TYPE_LONGLONG:
		c = 'n';
		break;
	case ATOMIC_TYPE_ULONGLONG:
		c = 'o';
		break;
	case ATOMIC_TYPE_FLOAT:
		c = 'f';
		break;
	case ATOMIC_TYPE_DOUBLE:
		c = 'd';
		break;
	default:
		abort();
		break;
	}

	obstack_1grow(obst, c);
}

static
void mangle_compound_type(struct obstack *obst, const compound_type_t *type)
{
	const char *string     = type->symbol->string;
	size_t      string_len = strlen(string);
	obstack_printf(obst, "%zu%s", string_len, string);
}

static
void mangle_pointer_type(struct obstack *obst, const pointer_type_t *type)
{
	obstack_1grow(obst, 'P');
	mangle_type(obst, type->points_to);
}

static
void mangle_method_type(struct obstack *obst, const method_type_t *type)
{
	obstack_1grow(obst, 'F');
	mangle_type(obst, type->result_type);

	method_parameter_type_t *parameter_type = type->parameter_types;
	while(parameter_type != NULL) {
		mangle_type(obst, parameter_type->type);
	}
	obstack_1grow(obst, 'E');
}

static
void mangle_reference_type_variable(struct obstack *obst, 
                                    const type_reference_t* ref)
{
	type_variable_t *type_var     = ref->r.type_variable;
	type_t          *current_type = type_var->current_type;

	if(current_type == NULL) {
		panic("can't mangle unbound type variable");
	}
	mangle_type(obst, current_type);
}

void mangle_type(struct obstack *obst, const type_t *type)
{
	switch(type->type) {
	case TYPE_INVALID:
		abort();
		break;
	case TYPE_VOID:
		obstack_1grow(obst, 'v');
		break;
	case TYPE_ATOMIC:
		mangle_atomic_type(obst, (const atomic_type_t*) type);
		break;
	case TYPE_COMPOUND:
		mangle_compound_type(obst, (const compound_type_t*) type);
		break;
	case TYPE_METHOD:
		mangle_method_type(obst, (const method_type_t*) type);
		break;
	case TYPE_POINTER:
		mangle_pointer_type(obst, (const pointer_type_t*) type);
		break;
	case TYPE_REFERENCE_TYPE_VARIABLE:
		mangle_reference_type_variable(obst, (const type_reference_t*) type);
		break;

	default:
		abort();
	}
}

