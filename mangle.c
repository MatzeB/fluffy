#include <config.h>

#include <stdbool.h>
#include "mangle.h"
#include "ast_t.h"
#include "type_t.h"
#include "adt/error.h"
#include <libfirm/firm.h>
#include "driver/firm_cmdline.h"

static struct obstack obst;
static bool add_underscore_prefix;

static void mangle_string(const char *str)
{
	size_t len = strlen(str);
	obstack_grow(&obst, str, len);
}

static void mangle_len_string(const char *string)
{
	size_t len = strlen(string);
	obstack_printf(&obst, "%zu%s", len, string);
}

static void mangle_atomic_type(const atomic_type_t *type)
{
	char c;

	switch (type->akind) {
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

	obstack_1grow(&obst, c);
}

void set_add_underscore_prefix(bool new_add_underscore_prefix)
{
	add_underscore_prefix = new_add_underscore_prefix;
}

static void mangle_type_variables(type_variable_t *type_variables)
{
	type_variable_t *type_variable = type_variables;
	for ( ; type_variable != NULL; type_variable = type_variable->next) {
		/* is this a good char? */
		obstack_1grow(&obst, 'T');
		mangle_type(type_variable->current_type);
	}
}

static void mangle_compound_type(const compound_type_t *type)
{
	mangle_len_string(type->symbol->string);
	mangle_type_variables(type->type_parameters);
}

static void mangle_pointer_type(const pointer_type_t *type)
{
	obstack_1grow(&obst, 'P');
	mangle_type(type->points_to);
}

static void mangle_array_type(const array_type_t *type)
{
	obstack_1grow(&obst, 'A');
	mangle_type(type->element_type);
	int size = fold_constant_to_int(type->size_expression);
	obstack_printf(&obst, "%lu", size);
}

static void mangle_function_type(const function_type_t *type)
{
	obstack_1grow(&obst, 'F');
	mangle_type(type->result_type);

	function_parameter_type_t *parameter_type = type->parameter_types;
	while (parameter_type != NULL) {
		mangle_type(parameter_type->type);
	}
	obstack_1grow(&obst, 'E');
}

static void mangle_reference_type_variable(const type_reference_t* ref)
{
	type_variable_t *type_var     = ref->type_variable;
	type_t          *current_type = type_var->current_type;

	if (current_type == NULL) {
		panic("can't mangle unbound type variable");
	}
	mangle_type(current_type);
}

static void mangle_bind_typevariables(const bind_typevariables_type_t *type)
{
	compound_type_t *polymorphic_type = type->polymorphic_type;

	int old_top = typevar_binding_stack_top();
	push_type_variable_bindings(polymorphic_type->type_parameters,
	                            type->type_arguments);
	mangle_type((type_t*) polymorphic_type);
	pop_type_variable_bindings(old_top);
}

void mangle_type(const type_t *type)
{
	switch (type->kind) {
	case TYPE_INVALID:
		break;
	case TYPE_VOID:
		obstack_1grow(&obst, 'v');
		return;
	case TYPE_ATOMIC:
		mangle_atomic_type((const atomic_type_t*) type);
		return;
	case TYPE_TYPEOF: {
		const typeof_type_t *typeof_type = (const typeof_type_t*) type;
		mangle_type(typeof_type->expression->base.type);
		return;
	}
	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		mangle_compound_type((const compound_type_t*) type);
		return;
	case TYPE_FUNCTION:
		mangle_function_type((const function_type_t*) type);
		return;
	case TYPE_POINTER:
		mangle_pointer_type((const pointer_type_t*) type);
		return;
	case TYPE_ARRAY:
		mangle_array_type((const array_type_t*) type);
		return;
	case TYPE_REFERENCE:
		panic("can't mangle unresolved type reference");
		return;
	case TYPE_BIND_TYPEVARIABLES:
		mangle_bind_typevariables((const bind_typevariables_type_t*) type);
		return;
	case TYPE_REFERENCE_TYPE_VARIABLE:
		mangle_reference_type_variable((const type_reference_t*) type);
		return;
	case TYPE_ERROR:
		panic("trying to mangle error type");
	}
	panic("Unknown type mangled");
}

void mangle_symbol_simple(symbol_t *symbol)
{
	mangle_string(symbol->string);
}

void mangle_symbol(symbol_t *symbol)
{
	mangle_len_string(symbol->string);
}

void mangle_concept_name(symbol_t *symbol)
{
	obstack_grow(&obst, "tcv", 3);
	mangle_len_string(symbol->string);
}

void start_mangle(void)
{
	if (add_underscore_prefix) {
		obstack_1grow(&obst, '_');
	}
}

ident *finish_mangle(void)
{
	size_t  size = obstack_object_size(&obst);
	char   *str  = obstack_finish(&obst);
	ident  *id   = new_id_from_chars(str, size);
	obstack_free(&obst, str);
	return id;
}

void init_mangle(void)
{
	obstack_init(&obst);
}

void exit_mangle(void)
{
	obstack_free(&obst, NULL);
}
