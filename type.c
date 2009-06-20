#include <config.h>

#include "type_t.h"
#include "ast_t.h"
#include "type_hash.h"
#include "adt/error.h"
#include "adt/array.h"

//#define DEBUG_TYPEVAR_BINDING

typedef struct typevar_binding_t typevar_binding_t;
struct typevar_binding_t {
	type_variable_t *type_variable;
	type_t          *old_current_type;
};

static typevar_binding_t *typevar_binding_stack = NULL;

static struct obstack  _type_obst;
struct obstack        *type_obst = &_type_obst;

static type_base_t  type_void_    = { TYPE_VOID, NULL };
static type_base_t  type_invalid_ = { TYPE_INVALID, NULL };
type_t             *type_void     = (type_t*) &type_void_;
type_t             *type_invalid  = (type_t*) &type_invalid_;

static FILE* out;

void init_type_module()
{
	obstack_init(type_obst);
	typevar_binding_stack = NEW_ARR_F(typevar_binding_t, 0);
	out = stderr;
}

void exit_type_module()
{
	DEL_ARR_F(typevar_binding_stack);
	obstack_free(type_obst, NULL);
}

static void print_atomic_type(const atomic_type_t *type)
{
	switch (type->akind) {
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

static void print_function_type(const function_type_t *type)
{
	fputs("<", out);
	fputs("func(", out);
	function_parameter_type_t *param_type = type->parameter_types;
	int first = 1;
	while (param_type != NULL) {
		if (first) {
			first = 0;
		} else {
			fputs(", ", out);
		}
		print_type(param_type->type);
		param_type = param_type->next;
	}
	fputs(")", out);

	if (type->result_type != NULL && type->result_type->kind != TYPE_VOID) {
		fputs(" : ", out);
		print_type(type->result_type);
	}
	fputs(">", out);
}

static void print_pointer_type(const pointer_type_t *type)
{
	print_type(type->points_to);
	fputs("*", out);
}

static void print_array_type(const array_type_t *type)
{
	print_type(type->element_type);
	fputs("[", out);
	print_expression(type->size_expression);
	fputs("]", out);
}

static void print_type_reference(const type_reference_t *type)
{
	fprintf(out, "<?%s>", type->symbol->string);
}

static void print_type_variable(const type_variable_t *type_variable)
{
	if (type_variable->current_type != NULL) {
		print_type(type_variable->current_type);
		return;
	}

	fprintf(out, "%s:", type_variable->base.symbol->string);

	type_constraint_t *constraint = type_variable->constraints;
	int first = 1;
	while (constraint != NULL) {
		if (first) {
			first = 0;
		} else {
			fprintf(out, ", ");
		}
		fprintf(out, "%s", constraint->concept_symbol->string);
		
		constraint = constraint->next;
	}
}

static void print_type_reference_variable(const type_reference_t *type)
{
	type_variable_t *type_variable = type->type_variable;
	print_type_variable(type_variable);
}

static void print_compound_type(const compound_type_t *type)
{
	fprintf(out, "%s", type->symbol->string);

	type_variable_t *type_parameter = type->type_parameters;
	if (type_parameter != NULL) {
		fprintf(out, "<");
		while (type_parameter != NULL) {
			if (type_parameter != type->type_parameters) {
				fprintf(out, ", ");
			}
			print_type_variable(type_parameter);
			type_parameter = type_parameter->next;
		}
		fprintf(out, ">");
	}
}

static void print_bind_type_variables(const bind_typevariables_type_t *type)
{
	compound_type_t *polymorphic_type = type->polymorphic_type;

	int old_top = typevar_binding_stack_top();
	push_type_variable_bindings(polymorphic_type->type_parameters,
	                            type->type_arguments);

	print_type((type_t*) polymorphic_type);

	pop_type_variable_bindings(old_top);
}

void print_type(const type_t *type)
{
	if (type == NULL) {
		fputs("nil type", out);
		return;
	}

	switch (type->kind) {
	case TYPE_INVALID:
		fputs("invalid", out);
		return;
	case TYPE_TYPEOF: {
		const typeof_type_t *typeof_type = (const typeof_type_t*) type;
		fputs("typeof(", out);
		print_expression(typeof_type->expression);
		fputs(")", out);
		return;
	}
	case TYPE_ERROR:
		fputs("error", out);
		return;
	case TYPE_VOID:
		fputs("void", out);
		return;
	case TYPE_ATOMIC:
		print_atomic_type(&type->atomic);
		return;
	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		print_compound_type(&type->compound);
		return;
	case TYPE_FUNCTION:
		print_function_type(&type->function);
		return;
	case TYPE_POINTER:
		print_pointer_type(&type->pointer);
		return;
	case TYPE_ARRAY:
		print_array_type(&type->array);
		return;
	case TYPE_REFERENCE:
		print_type_reference(&type->reference);
		return;
	case TYPE_REFERENCE_TYPE_VARIABLE:
		print_type_reference_variable(&type->reference);
		return;
	case TYPE_BIND_TYPEVARIABLES:
		print_bind_type_variables(&type->bind_typevariables);
		return;
	}
	fputs("unknown", out);
}

int type_valid(const type_t *type)
{
	switch (type->kind) {
	case TYPE_INVALID:
	case TYPE_REFERENCE:
		return 0;
	default:
		return 1;
	}
}

int is_type_int(const type_t *type)
{
	if (type->kind != TYPE_ATOMIC)
		return 0;

	atomic_type_t *atomic_type = (atomic_type_t*) type;
	switch (atomic_type->akind) {
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

int is_type_numeric(const type_t *type)
{
	if (type->kind != TYPE_ATOMIC)
		return 0;

	atomic_type_t *atomic_type = (atomic_type_t*) type;
	switch (atomic_type->akind) {
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
	case ATOMIC_TYPE_FLOAT:
	case ATOMIC_TYPE_DOUBLE:
		return 1;
	default:
		return 0;
	}
}


type_t* make_atomic_type(atomic_type_kind_t akind)
{
	type_t *type = allocate_type(TYPE_ATOMIC);
	type->atomic.akind = akind;

	type_t *normalized_type = typehash_insert(type);
	if (normalized_type != type) {
		obstack_free(type_obst, type);
	}

	return normalized_type;
}

type_t* make_pointer_type(type_t *points_to)
{
	type_t *type = allocate_type(TYPE_POINTER);
	type->pointer.points_to = points_to;

	type_t *normalized_type = typehash_insert(type);
	if (normalized_type != type) {
		obstack_free(type_obst, type);
	}

	return normalized_type;
}

static type_t *create_concrete_compound_type(compound_type_t *type)
{
	/* TODO: handle structs with typevars */
	return (type_t*) type;
}

static type_t *create_concrete_function_type(function_type_t *type)
{
	int need_new_type = 0;

	type_t *new_type = allocate_type(TYPE_FUNCTION);
	
	type_t *result_type = create_concrete_type(type->result_type);
	if (result_type != type->result_type)
		need_new_type = 1;
	new_type->function.result_type = result_type;

	function_parameter_type_t *parameter_type      = type->parameter_types;
	function_parameter_type_t *last_parameter_type = NULL;
	while (parameter_type != NULL) {
		type_t *param_type     = parameter_type->type;
		type_t *new_param_type = create_concrete_type(param_type);

		if (new_param_type != param_type)
			need_new_type = 1;

		function_parameter_type_t *new_parameter_type
			= obstack_alloc(type_obst, sizeof(new_parameter_type[0]));
		memset(new_parameter_type, 0, sizeof(new_parameter_type[0]));
		new_parameter_type->type = new_param_type;

		if (last_parameter_type != NULL) {
			last_parameter_type->next = new_parameter_type;
		} else {
			new_type->function.parameter_types = new_parameter_type;
		}
		last_parameter_type = new_parameter_type;

		parameter_type = parameter_type->next;
	}

	if (!need_new_type) {
		obstack_free(type_obst, new_type);
		new_type = (type_t*) type;
	}

	return new_type;
}

static type_t *create_concrete_pointer_type(pointer_type_t *type)
{
	type_t *points_to = create_concrete_type(type->points_to);

	if (points_to == type->points_to)
		return (type_t*) type;

	type_t *new_type = allocate_type(TYPE_POINTER);
	new_type->pointer.points_to = points_to;

	type_t *normalized_type = typehash_insert((type_t*) new_type);
	if (normalized_type != new_type) {
		obstack_free(type_obst, new_type);
	}

	return normalized_type;
}

static type_t *create_concrete_type_variable_reference_type(type_reference_t *type)
{
	type_variable_t *type_variable = type->type_variable;
	type_t          *current_type  = type_variable->current_type;

	if (current_type != NULL)
		return current_type;

	return (type_t*) type;
}

static type_t *create_concrete_array_type(array_type_t *type)
{
	type_t *element_type = create_concrete_type(type->element_type);
	if (element_type == type->element_type)
		return (type_t*) type;

	type_t *new_type = allocate_type(TYPE_ARRAY);
	new_type->array.element_type    = element_type;
	new_type->array.size_expression = type->size_expression;

	type_t *normalized_type = typehash_insert((type_t*) new_type);
	if (normalized_type != (type_t*) new_type) {
		obstack_free(type_obst, new_type);
	}

	return normalized_type;
}

static type_t *create_concrete_typevar_binding_type(bind_typevariables_type_t *type)
{
	int changed = 0;

	type_argument_t *new_arguments;
	type_argument_t *last_argument = NULL;
	type_argument_t *type_argument = type->type_arguments;
	while (type_argument != NULL) {
		type_t *type     = type_argument->type;
		type_t *new_type = create_concrete_type(type);

		if (new_type != type) {
			changed = 1;
		}

		type_argument_t *new_argument 
			= obstack_alloc(type_obst, sizeof(new_argument[0]));
		memset(new_argument, 0, sizeof(new_argument[0]));
		new_argument->type = new_type;
		if (last_argument != NULL) {
			last_argument->next = new_argument;
		} else {
			new_arguments = new_argument;
		}
		last_argument = new_argument;

		type_argument = type_argument->next;
	}

	if (!changed) {
		assert(new_arguments != NULL);
		obstack_free(type_obst, new_arguments);
		return (type_t*) type;
	}

	type_t *new_type = allocate_type(TYPE_BIND_TYPEVARIABLES);
	new_type->bind_typevariables.polymorphic_type = type->polymorphic_type;
	new_type->bind_typevariables.type_arguments   = new_arguments;

	type_t *normalized_type = typehash_insert(new_type);
	if (normalized_type != new_type) {
		obstack_free(type_obst, new_type);
	}

	return normalized_type;
}

type_t *create_concrete_type(type_t *type)
{
	switch (type->kind) {
	case TYPE_INVALID:
		return type_invalid;
	case TYPE_VOID:
		return type_void;
	case TYPE_ERROR:
	case TYPE_ATOMIC:
		return type;
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return create_concrete_compound_type((compound_type_t*) type);
	case TYPE_FUNCTION:
		return create_concrete_function_type((function_type_t*) type);
	case TYPE_POINTER:
		return create_concrete_pointer_type((pointer_type_t*) type);
	case TYPE_ARRAY:
		return create_concrete_array_type((array_type_t*) type);
	case TYPE_REFERENCE_TYPE_VARIABLE:
		return create_concrete_type_variable_reference_type(
				(type_reference_t*) type);
	case TYPE_BIND_TYPEVARIABLES:
		return create_concrete_typevar_binding_type(
		        (bind_typevariables_type_t*) type);
	case TYPE_TYPEOF:
		panic("TODO: concrete type for typeof()");
	case TYPE_REFERENCE:
		panic("trying to normalize unresolved type reference");
		break;
	}

	return type;
}

int typevar_binding_stack_top()
{
	return ARR_LEN(typevar_binding_stack);
}

void push_type_variable_bindings(type_variable_t *type_parameters,
                                 type_argument_t *type_arguments)
{
	type_variable_t *type_parameter;
	type_argument_t *type_argument;

	if (type_parameters == NULL || type_arguments == NULL)
		return;

	/* we have to take care that all rebinding happens atomically, so we first
	 * create the structures on the binding stack and misuse the
	 * old_current_type value to temporarily save the new! current_type.
	 * We can then walk the list and set the new types */
	type_parameter = type_parameters;
	type_argument  = type_arguments;

	int old_top = typevar_binding_stack_top();
	int top     = ARR_LEN(typevar_binding_stack) + 1;
	while (type_parameter != NULL) {
		type_t *type = type_argument->type;
		while (type->kind == TYPE_REFERENCE_TYPE_VARIABLE) {
			type_reference_t *ref = (type_reference_t*) type;
			type_variable_t  *var = ref->type_variable;

			if (var->current_type == NULL) {
				break;
			}
			type = var->current_type;
		}

		top = ARR_LEN(typevar_binding_stack) + 1;
		ARR_RESIZE(typevar_binding_t, typevar_binding_stack, top);

		typevar_binding_t *binding = & typevar_binding_stack[top-1];
		binding->type_variable     = type_parameter;
		binding->old_current_type  = type;

		type_parameter = type_parameter->next;
		type_argument  = type_argument->next;
	}
	assert(type_parameter == NULL && type_argument == NULL);

	for (int i = old_top+1; i <= top; ++i) {
		typevar_binding_t *binding       = & typevar_binding_stack[i-1];
		type_variable_t   *type_variable = binding->type_variable;
		type_t            *new_type      = binding->old_current_type;

		binding->old_current_type   = type_variable->current_type;
		type_variable->current_type = new_type;

#ifdef DEBUG_TYPEVAR_BINDING
		fprintf(stderr, "binding '%s'(%p) to ", type_variable->symbol->string,
		        type_variable);
		print_type(stderr, type_variable->current_type);
		fprintf(stderr, "\n");
#endif
	}
}

void pop_type_variable_bindings(int new_top)
{
	int top = ARR_LEN(typevar_binding_stack) - 1;
	for (int i = top; i >= new_top; --i) {
		typevar_binding_t *binding       = & typevar_binding_stack[i];
		type_variable_t   *type_variable = binding->type_variable;
		type_variable->current_type      = binding->old_current_type;

#ifdef DEBUG_TYPEVAR_BINDING
		fprintf(stderr, "reset binding of '%s'(%p) to ",
		        type_variable->symbol->string, type_variable);
		print_type(stderr, binding->old_current_type);
		fprintf(stderr, "\n");
#endif
	}

	ARR_SHRINKLEN(typevar_binding_stack, new_top);
}

type_t *skip_typeref(type_t *type)
{
	if (type->kind == TYPE_TYPEOF) {
		typeof_type_t *typeof_type = &type->typeof;
		return skip_typeref(typeof_type->expression->base.type);
	}
	return type;
}
