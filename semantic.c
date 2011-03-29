#include <config.h>

#include <stdbool.h>

#include "semantic_t.h"

#include "ast_t.h"
#include "type_t.h"
#include "type_hash.h"
#include "match_type.h"
#include "adt/obst.h"
#include "adt/array.h"
#include "adt/error.h"

//#define DEBUG_TYPEVAR_BINDINGS
//#define ABORT_ON_ERRORS
//#define DEBUG_ENVIRONMENT

typedef struct environment_entry_t environment_entry_t;
struct environment_entry_t {
	symbol_t    *symbol;
	entity_t    *up;
	const void  *up_context;
};

static lower_statement_function  *statement_lowerers  = NULL;
static lower_expression_function *expression_lowerers = NULL;

static struct obstack        symbol_environment_obstack;
static environment_entry_t **symbol_stack;
static bool                  found_export;
static bool                  found_errors;

static type_t *type_bool     = NULL;
static type_t *type_byte     = NULL;
static type_t *type_int      = NULL;
static type_t *type_uint     = NULL;
static type_t *type_double   = NULL;
static type_t *type_byte_ptr = NULL;
static type_t *type_void_ptr = NULL;
static type_t *error_type    = NULL;


static function_t *current_function          = NULL;
bool               last_statement_was_return = false;

static void check_and_push_context(context_t *context);

static void check_function(function_t *function, symbol_t *symbol,
                           const source_position_t source_position);

static void resolve_function_types(function_t *function);

void print_error_prefix(const source_position_t position)
{
	fprintf(stderr, "%s:%d: error: ", position.input_name, position.linenr);
	found_errors = true;
#ifdef ABORT_ON_ERRORS
	abort();
#endif
}

void print_warning_prefix(const source_position_t position)
{
	fprintf(stderr, "%s:%d: warning: ", position.input_name, position.linenr);
}

void error_at(const source_position_t position,
              const char *message)
{
	print_error_prefix(position);
	fprintf(stderr, "%s\n", message);
}

/**
 * pushs an environment_entry on the environment stack and links the
 * corresponding symbol to the new entry
 */
static void environment_push(entity_t *entity, const void *context)
{
	environment_entry_t *entry 
		= obstack_alloc(&symbol_environment_obstack, sizeof(entry[0]));
	memset(entry, 0, sizeof(entry[0]));

	int top = ARR_LEN(symbol_stack);
	ARR_RESIZE(environment_entry_t*, symbol_stack, top + 1);
	symbol_stack[top] = entry;

	symbol_t *symbol = entity->base.symbol;

	assert(entity != symbol->entity);

	if (symbol->context == context) {
		assert(symbol->entity != NULL);
		print_error_prefix(entity->base.source_position);
		fprintf(stderr, "multiple definitions for symbol '%s'.\n",
		        symbol->string);
		print_error_prefix(symbol->entity->base.source_position);
		fprintf(stderr, "this is the location of the previous entity.\n");
	}

#ifdef DEBUG_ENVIRONMENT
	fprintf(stderr, "Push symbol '%s'\n", symbol->string);
#endif

	entry->up         = symbol->entity;
	entry->up_context = symbol->context;
	entry->symbol     = symbol;
	symbol->entity    = entity;
	symbol->context   = context;
}

/**
 * pops symbols from the environment stack until @p new_top is the top element
 */
static inline
void environment_pop_to(size_t new_top)
{
	environment_entry_t *entry = NULL;
	size_t top = ARR_LEN(symbol_stack);
	size_t i;

	if (new_top == top)
		return;

	assert(new_top < top);
	i = top;
	do {
		entry  = symbol_stack[i - 1];

		symbol_t *symbol = entry->symbol;
		entity_t *entity = symbol->entity;

		if (entity->base.refs == 0 && !entity->base.exported) {
			switch (entity->kind) {
			/* only warn for functions/variables at the moment, we don't
			   count refs on types yet */
			case ENTITY_FUNCTION:
			case ENTITY_VARIABLE:
				print_warning_prefix(entity->base.source_position);
				fprintf(stderr, "%s '%s' was declared but never read\n",
						get_entity_kind_name(entity->kind), symbol->string);
			default:
				break;
			}
		}

#ifdef DEBUG_ENVIRONMENT
		fprintf(stderr, "Pop symbol '%s'\n", symbol->string);
#endif

		symbol->entity  = entry->up;
		symbol->context = entry->up_context;

		--i;
	} while (i != new_top);
	obstack_free(&symbol_environment_obstack, entry);

	ARR_SHRINKLEN(symbol_stack, (int) new_top);
}

/**
 * returns the top element of the environment stack
 */
static inline
size_t environment_top(void)
{
	return ARR_LEN(symbol_stack);
}

static type_t *normalize_type(type_t *type);

static void normalize_type_arguments(type_argument_t *type_arguments)
{
	/* normalize type arguments */
	type_argument_t *type_argument = type_arguments;
	while (type_argument != NULL) {
		type_argument->type = normalize_type(type_argument->type);

		type_argument = type_argument->next;
	}
}

static type_t *resolve_type_reference(type_reference_t *type_ref)
{
	normalize_type_arguments(type_ref->type_arguments);

	symbol_t *symbol = type_ref->symbol;
	entity_t *entity = symbol->entity;
	if (entity == NULL) {
		print_error_prefix(type_ref->source_position);
		fprintf(stderr, "can't resolve type: symbol '%s' is unknown\n",
		        symbol->string);
		return type_invalid;
	}

	if (entity->kind == ENTITY_TYPE_VARIABLE) {
		type_variable_t *type_variable = &entity->type_variable;

		if (type_variable->current_type != NULL) {
			/* not sure if this is really a problem... */
			fprintf(stderr, "Debug warning: unresolved type var ref found "
			        "a concrete type...\n");
			return type_variable->current_type;
		}
		type_ref->base.kind     = TYPE_REFERENCE_TYPE_VARIABLE;
		type_ref->type_variable = type_variable;
		return typehash_insert((type_t*) type_ref);
	}

	if (entity->kind != ENTITY_TYPEALIAS) {
		print_error_prefix(type_ref->source_position);
		fprintf(stderr, "expected a type alias or type variable, but '%s' is a '%s'\n",
		        symbol->string, get_entity_kind_name(entity->kind));
		return type_invalid;
	}

	typealias_t *typealias = &entity->typealias;
	typealias->type        = normalize_type(typealias->type);

	type_t          *type = typealias->type;
	type_variable_t *type_parameters = NULL;
	compound_type_t *compound_type   = NULL;
	if (type->kind == TYPE_COMPOUND_STRUCT || type->kind == TYPE_COMPOUND_UNION) {
		compound_type   = (compound_type_t*) type;
		type_parameters = compound_type->type_parameters;
	}

	/* check that type arguments match type parameters 
	 * and normalize the type arguments */
	type_argument_t *type_arguments = type_ref->type_arguments;
	type_variable_t *type_parameter = type_parameters;
	type_argument_t *type_argument  = type_arguments;
	while (type_parameter != NULL) {
		if (type_argument == NULL) {
			print_error_prefix(type_ref->source_position);
			fprintf(stderr, "too few type parameters specified for type ");
			print_type(type);
			fprintf(stderr, "\n");
			break;
		}
		
		type_parameter = type_parameter->next;
		type_argument  = type_argument->next;
	}
	if (type_argument != NULL) {
		print_error_prefix(type_ref->source_position);
		if (type_parameters == NULL) {
			fprintf(stderr, "type ");
		} else {
			fprintf(stderr, "too many type parameters specified for ");
		}
		print_type(type);
		fprintf(stderr, " takes no type parameters\n");
	}

	if (type_parameters != NULL && type_argument == NULL
			&& type_argument == NULL) {
		bind_typevariables_type_t *bind_typevariables
			= obstack_alloc(type_obst, sizeof(bind_typevariables[0]));
		memset(bind_typevariables, 0, sizeof(bind_typevariables[0]));

		bind_typevariables->base.kind        = TYPE_BIND_TYPEVARIABLES;
		bind_typevariables->type_arguments   = type_arguments;
		assert(compound_type != NULL);
		bind_typevariables->polymorphic_type = compound_type;

		type = (type_t*) bind_typevariables;
	}

	return type;
}

static type_t *resolve_type_reference_type_var(type_reference_t *type_ref)
{
	type_variable_t *type_variable = type_ref->type_variable;
	if (type_variable->current_type != NULL) {
		return normalize_type(type_variable->current_type);
	}

	return typehash_insert((type_t*) type_ref);
}

static type_t *normalize_pointer_type(pointer_type_t *type)
{
	type->points_to = normalize_type(type->points_to);

	return typehash_insert((type_t*) type);
}

static type_t *normalize_array_type(array_type_t *type)
{
	type->element_type = normalize_type(type->element_type);

	type->size_expression = check_expression(type->size_expression);

	return typehash_insert((type_t*) type);
}

static type_t *normalize_function_type(function_type_t *function_type)
{
	function_type->result_type = normalize_type(function_type->result_type);

	function_parameter_type_t *parameter = function_type->parameter_types;
	while (parameter != NULL) {
		parameter->type = normalize_type(parameter->type);

		parameter = parameter->next;
	}

	return typehash_insert((type_t*) function_type);
}

static void check_compound_type(compound_type_t *type)
{
	int old_top = environment_top();

	check_and_push_context(&type->context);

	compound_entry_t *entry = type->entries;
	while (entry != NULL) {
		type_t *type = entry->type;
		if (type->kind == TYPE_COMPOUND_STRUCT
				|| type->kind == TYPE_COMPOUND_UNION) {
			compound_type_t *compound_type = (compound_type_t*) type;
			check_compound_type(compound_type);
		}
		entry->type = normalize_type(type);

		entry = entry->next;
	}

	environment_pop_to(old_top);
}

static type_t *normalize_compound_type(compound_type_t *type)
{
	type_t *result = typehash_insert((type_t*) type);

	return result;
}

static type_t *normalize_bind_typevariables(bind_typevariables_type_t *type)
{
	type_t *polymorphic_type = (type_t*) type->polymorphic_type;
	polymorphic_type = normalize_type(polymorphic_type);
	assert(polymorphic_type->kind == TYPE_COMPOUND_STRUCT ||
			polymorphic_type->kind == TYPE_COMPOUND_UNION);
	type->polymorphic_type = (compound_type_t*) polymorphic_type;

	type_t *result = typehash_insert((type_t*) type);
	return result;
}

static type_t *normalize_type(type_t *type)
{
	/* happens sometimes on semantic errors */
	if (type == NULL)
		return NULL;

	switch (type->kind) {
	case TYPE_INVALID:
	case TYPE_VOID:
	case TYPE_ATOMIC:
	case TYPE_ERROR:
		return type;

	case TYPE_TYPEOF: {
		typeof_type_t *typeof_type = (typeof_type_t*) type;
		typeof_type->expression = check_expression(typeof_type->expression);
		return type;
	}

	case TYPE_REFERENCE:
		return resolve_type_reference((type_reference_t*) type);

	case TYPE_REFERENCE_TYPE_VARIABLE:
		return resolve_type_reference_type_var((type_reference_t*) type);

	case TYPE_POINTER:
		return normalize_pointer_type((pointer_type_t*) type);

	case TYPE_ARRAY:
		return normalize_array_type((array_type_t*) type);
	
	case TYPE_FUNCTION:
		return normalize_function_type((function_type_t*) type);

	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		return normalize_compound_type((compound_type_t*) type);

	case TYPE_BIND_TYPEVARIABLES:
		return normalize_bind_typevariables((bind_typevariables_type_t*) type);
	}

	panic("Unknown type found");
}

static type_t *check_reference(entity_t *entity,
                               const source_position_t source_position)
{
	type_t *type;
	entity->base.refs++;

	switch (entity->kind) {
	case ENTITY_VARIABLE:
		type = entity->variable.type;
		if (type == NULL)
			return NULL;

		if (type->kind == TYPE_COMPOUND_STRUCT
				|| type->kind == TYPE_COMPOUND_UNION
				|| type->kind == TYPE_BIND_TYPEVARIABLES
				|| type->kind == TYPE_ARRAY) {
			entity->variable.needs_entity = true;
		}
		return type;
	case ENTITY_FUNCTION:
		return make_pointer_type((type_t*) entity->function.function.type);
	case ENTITY_CONSTANT: {
		constant_t *constant = &entity->constant;
		/* do type inference for the constant if needed */
		if (constant->type == NULL) {
			constant->expression = check_expression(constant->expression);
			constant->type       = constant->expression->base.type;
		}
		return constant->type;
	}
	case ENTITY_FUNCTION_PARAMETER:
		assert(entity->parameter.type != NULL);
		return entity->parameter.type;
	case ENTITY_CONCEPT_FUNCTION:
		return make_pointer_type((type_t*) entity->concept_function.type);
	case ENTITY_LABEL:
	case ENTITY_TYPEALIAS:
	case ENTITY_CONCEPT:
	case ENTITY_TYPE_VARIABLE:
		print_error_prefix(source_position);
		fprintf(stderr, "'%s' (a '%s') can't be used as expression\n",
		        entity->base.symbol->string,
		        get_entity_kind_name(entity->kind));
		return NULL;
	case ENTITY_ERROR:
		found_errors = true;
		return NULL;
	case ENTITY_INVALID:
		panic("reference to invalid declaration type encountered");
	}
	panic("reference to unknown declaration type encountered");
}

static entity_t *create_error_entity(symbol_t *symbol)
{
	entity_t *entity = allocate_entity(ENTITY_ERROR);
	entity->base.symbol   = symbol;
	entity->base.exported = true;
	return entity;
}

static void check_reference_expression(reference_expression_t *ref)
{
	symbol_t *symbol = ref->symbol;
	entity_t *entity = symbol->entity;
	if (entity == NULL) {
		print_error_prefix(ref->base.source_position);
		fprintf(stderr, "no known definition for '%s'\n", symbol->string);
		entity = create_error_entity(symbol);
	}

	normalize_type_arguments(ref->type_arguments);

	ref->entity    = entity;
	type_t *type   = check_reference(entity, ref->base.source_position);
	ref->base.type = type;
}

static bool is_lvalue(const expression_t *expression)
{
	switch (expression->kind) {
	case EXPR_REFERENCE: {
		const reference_expression_t *reference
			= (const reference_expression_t*) expression;
		const entity_t *entity = reference->entity;
		if (entity->kind == ENTITY_VARIABLE) {
			return true;
		}
		break;
	}
	case EXPR_ARRAY_ACCESS:
		return true;
	case EXPR_SELECT:
		return true;
	case EXPR_UNARY_DEREFERENCE:
		return true;
	default:
		break;
	}

	return false;
}

static void check_assign_expression(binary_expression_t *assign)
{
	expression_t *left  = assign->left;
	expression_t *right = assign->right;

	if (!is_lvalue(left)) {
		error_at(assign->base.source_position,
		         "left side of assign is not an lvalue.\n");
		return;
	}
	if (left->kind == EXPR_REFERENCE) {
		reference_expression_t *reference = (reference_expression_t*) left;
		entity_t               *entity    = reference->entity;

		if (entity->kind == ENTITY_VARIABLE) {
			variable_t *variable = (variable_t*) entity;
			symbol_t   *symbol   = variable->base.symbol;

			/* do type inference if needed */
			if (left->base.type == NULL) {
				if (right->base.type == NULL) {
					print_error_prefix(assign->base.source_position);
					fprintf(stderr, "can't infer type for '%s'\n",
					        symbol->string);
					return;
				}

				variable->type  = right->base.type;
				left->base.type = right->base.type;
			}

			/* the reference expression increased the ref pointer, but
			 * making an assignment is not reading the value */
			variable->base.refs--;
		}
	}
}

/**
 * creates an implicit cast if possible or reports an error
 */
static expression_t *make_cast(expression_t *from,
                               type_t *dest_type,
                               const source_position_t source_position,
							   bool lenient)
{
	if (dest_type == NULL || from->base.type == dest_type)
		return from;

	/* TODO: - test which types can be implicitely casted... 
	 *       - improve error reporting (want to know the context of the cast)
	 *          ("can't implicitely cast for argument 2 of function call...")
	 */
	dest_type = skip_typeref(dest_type);

	type_t *from_type = from->base.type;
	if (from_type == NULL) {
		print_error_prefix(from->base.source_position);
		fprintf(stderr, "can't implicitely cast from unknown type to ");
		print_type(dest_type);
		fprintf(stderr, "\n");
		return NULL;
	}

	from_type = skip_typeref(from_type);

	bool implicit_cast_allowed = true;
	if (from_type->kind == TYPE_POINTER) {
		if (dest_type->kind == TYPE_POINTER) {
			pointer_type_t *p1 = (pointer_type_t*) from_type;
			pointer_type_t *p2 = (pointer_type_t*) dest_type;
			/* you can implicitely cast any pointer to void* and
			 * it is allowed to cast 'null' to any pointer */
			if (p1->points_to == p2->points_to
					|| dest_type == type_void_ptr
					|| from->kind == EXPR_NULL_POINTER) {
				/* fine */
			} else if (is_type_array(p1->points_to)) {
				array_type_t *array_type = (array_type_t*) p1->points_to;
				if (array_type->element_type == p2->points_to) {
					/* fine */
				} else {
					implicit_cast_allowed = false;
				}
			} else {
				implicit_cast_allowed = false;
			}
		} else {
			implicit_cast_allowed = false;
		}
	} else if (from_type->kind == TYPE_ARRAY) {
		array_type_t *array_type = (array_type_t*) from_type;
		if (dest_type->kind == TYPE_POINTER) {
			pointer_type_t *pointer_type = (pointer_type_t*) dest_type;
			/* we can cast to pointer of same type and void* */
			if (pointer_type->points_to != array_type->element_type &&
					dest_type != type_void_ptr) {
				implicit_cast_allowed = false;
			}
		} else {
			implicit_cast_allowed = false;
		}
	} else if (dest_type->kind == TYPE_POINTER) {
		implicit_cast_allowed = false;
	} else if (from_type->kind == TYPE_ATOMIC) {
		if (dest_type->kind != TYPE_ATOMIC) {
			implicit_cast_allowed = false;
		} else {
			atomic_type_t      *from_type_atomic = &from_type->atomic;
			atomic_type_kind_t  from_akind       = from_type_atomic->akind;
			atomic_type_t      *dest_type_atomic = &dest_type->atomic;
			atomic_type_kind_t  dest_akind       = dest_type_atomic->akind;

			switch (from_akind) {
			case ATOMIC_TYPE_BOOL:
				if (!lenient) {
					implicit_cast_allowed = false;
					break;
				}
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_BYTE) ||
					(dest_akind == ATOMIC_TYPE_UBYTE);
			case ATOMIC_TYPE_UBYTE:
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_USHORT) ||
					(dest_akind == ATOMIC_TYPE_SHORT);
			case ATOMIC_TYPE_USHORT:
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_UINT) ||
					(dest_akind == ATOMIC_TYPE_INT);
			case ATOMIC_TYPE_UINT:
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_ULONG) ||
					(dest_akind == ATOMIC_TYPE_LONG);
			case ATOMIC_TYPE_ULONG:
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_ULONGLONG) ||
					(dest_akind == ATOMIC_TYPE_LONGLONG);
				break;
			case ATOMIC_TYPE_BYTE:
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_SHORT);
			case ATOMIC_TYPE_SHORT:
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_INT);
			case ATOMIC_TYPE_INT:
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_LONG);
			case ATOMIC_TYPE_LONG:
				implicit_cast_allowed |=
					(dest_akind == ATOMIC_TYPE_LONGLONG);
				break;

			case ATOMIC_TYPE_FLOAT:
				implicit_cast_allowed = (dest_akind == ATOMIC_TYPE_DOUBLE);
				break;

			case ATOMIC_TYPE_DOUBLE:
			case ATOMIC_TYPE_LONGLONG:
			case ATOMIC_TYPE_ULONGLONG:
			case ATOMIC_TYPE_INVALID:
				implicit_cast_allowed = false;
				break;
			}
		}
	}

	if (!implicit_cast_allowed) {
		print_error_prefix(source_position);
		fprintf(stderr, "can't implicitely cast ");
		print_type(from_type);
		fprintf(stderr, " to ");
		print_type(dest_type);
		fprintf(stderr, "\n");
		return NULL;
	}

	expression_t *cast         = allocate_expression(EXPR_UNARY_CAST);
	cast->base.source_position = source_position;
	cast->base.type            = dest_type;
	cast->unary.value          = from;
	return cast;
}

static expression_t *lower_sub_expression(expression_t *expression)
{
	binary_expression_t *sub = (binary_expression_t*) expression;

	expression_t *left      = check_expression(sub->left);
	expression_t *right     = check_expression(sub->right);
	type_t       *lefttype  = left->base.type;
	type_t       *righttype = right->base.type;	

	if (lefttype->kind != TYPE_POINTER && righttype->kind != TYPE_POINTER)
		return expression;

	sub->base.type = type_uint;

	pointer_type_t *p1 = (pointer_type_t*) lefttype;

	expression_t *sizeof_expr = allocate_expression(EXPR_SIZEOF);
	sizeof_expr->base.type    = type_uint;
	sizeof_expr->sizeofe.type = p1->points_to;

	expression_t *divexpr = allocate_expression(EXPR_BINARY_DIV);
	divexpr->base.type    = type_uint;
	divexpr->binary.left  = expression;
	divexpr->binary.right = sizeof_expr;

	sub->base.lowered = true;
	return divexpr;
}

static void check_binary_expression(binary_expression_t *binexpr)
{
	binexpr->left       = check_expression(binexpr->left);
	binexpr->right      = check_expression(binexpr->right);
	expression_t *left  = binexpr->left;
	expression_t *right = binexpr->right;

	type_t *exprtype;
	type_t *lefttype, *righttype;
	expression_kind_t kind = binexpr->base.kind;

	switch (kind) {
	case EXPR_BINARY_ASSIGN:
		check_assign_expression(binexpr);
		exprtype  = left->base.type;
		lefttype  = exprtype;
		righttype = exprtype;
		break;
	case EXPR_BINARY_ADD:
	case EXPR_BINARY_SUB:
		exprtype  = left->base.type;
		lefttype  = exprtype;
		righttype = right->base.type;
		/* implement address arithmetic */
		if (lefttype->kind == TYPE_POINTER && is_type_int(righttype)) {
			pointer_type_t *pointer_type = (pointer_type_t*) lefttype;

			expression_t *sizeof_expr = allocate_expression(EXPR_SIZEOF);
			sizeof_expr->base.type    = type_uint;
			sizeof_expr->sizeofe.type = pointer_type->points_to;

			expression_t *mulexpr = allocate_expression(EXPR_BINARY_MUL);
			mulexpr->base.type    = type_uint;
			mulexpr->binary.left  = make_cast(right, type_uint,
			                                  binexpr->base.source_position,
			                                  false);
			mulexpr->binary.right = sizeof_expr;

			expression_t *cast         = allocate_expression(EXPR_UNARY_CAST);
			cast->base.source_position = binexpr->base.source_position;
			cast->base.type            = lefttype;
			cast->unary.value          = mulexpr;

			right          = cast;
			binexpr->right = cast;
		}
		if (lefttype->kind == TYPE_POINTER && righttype->kind == TYPE_POINTER) {
			pointer_type_t *p1 = (pointer_type_t*) lefttype;
			pointer_type_t *p2 = (pointer_type_t*) righttype;
			if (p1->points_to != p2->points_to) {
				print_error_prefix(binexpr->base.source_position);
				fprintf(stderr, "Can only subtract pointers to same type, but have type ");
				print_type(lefttype);
				fprintf(stderr, " and ");
				print_type(righttype);
				fprintf(stderr, "\n");
			}
			exprtype = type_uint;
		}
		righttype = lefttype;
		break;
	case EXPR_BINARY_MUL:
	case EXPR_BINARY_MOD:
	case EXPR_BINARY_DIV:
		if (!is_type_numeric(left->base.type)) {
			print_error_prefix(binexpr->base.source_position);
			fprintf(stderr, "Mul/Mod/Div expressions need a numeric type but "
			        "type ");
			print_type(left->base.type);
			fprintf(stderr, "is given\n");
		}
		exprtype  = left->base.type;
		lefttype  = exprtype;
		righttype = lefttype;
		break;

	case EXPR_BINARY_AND:
	case EXPR_BINARY_OR:
	case EXPR_BINARY_XOR:
		if (!is_type_int(left->base.type)) {
			print_error_prefix(binexpr->base.source_position);
			fprintf(stderr, "And/Or/Xor expressions need an integer type "
			        "but type ");
			print_type(left->base.type);
			fprintf(stderr, "is given\n");
		}
		exprtype  = left->base.type;
		lefttype  = exprtype;
		righttype = left->base.type;
		break;

	case EXPR_BINARY_SHIFTLEFT:
	case EXPR_BINARY_SHIFTRIGHT:
		if (!is_type_int(left->base.type)) {
			print_error_prefix(binexpr->base.source_position);
			fprintf(stderr, "ShiftLeft/ShiftRight expressions need an integer "
			        "type, but type ");
			print_type(left->base.type);
			fprintf(stderr, "is given\n");
		}
		exprtype  = left->base.type;
		lefttype  = exprtype;
		righttype = type_uint;
		break;

	/* comparison operation */
	case EXPR_BINARY_EQUAL:
	case EXPR_BINARY_NOTEQUAL:
	case EXPR_BINARY_LESS:
	case EXPR_BINARY_LESSEQUAL:
	case EXPR_BINARY_GREATER:
	case EXPR_BINARY_GREATEREQUAL:
		exprtype  = type_bool;
		/* TODO find out greatest common type... */
		lefttype  = left->base.type;
		righttype = left->base.type;
		break;
	case EXPR_BINARY_LAZY_AND:
	case EXPR_BINARY_LAZY_OR:
		exprtype  = type_bool;
		lefttype  = type_bool;
		righttype = type_bool;
		break;
	default:
		panic("invalid type in binexpr");
	}

	if (left == NULL || right == NULL)
		return;

	if (left->base.type != lefttype) {
		binexpr->left  = make_cast(left, lefttype,
		                           binexpr->base.source_position,
								   false);
	}
	if (right->base.type != righttype) {
		binexpr->right = make_cast(right, righttype,
		                           binexpr->base.source_position,
								   false);
	}
	binexpr->base.type = exprtype;
}

/**
 * find a concept instance matching the current type_variable configuration
 */
static concept_instance_t *_find_concept_instance(concept_t *concept,
                                                  const source_position_t *pos)
{
	concept_instance_t *instance;
	for ( instance = concept->instances; instance != NULL;
			instance = instance->next_in_concept) {
		assert(instance->concept == concept);

		type_argument_t *argument  = instance->type_arguments;
		type_variable_t *parameter = concept->type_parameters;
		bool             match     = true;
		while (argument != NULL && parameter != NULL) {
			if (parameter->current_type == NULL) {
				print_error_prefix(*pos);
				panic("type variable has no type set while searching "
				      "concept instance");
			}
			if (!match_variant_to_concrete_type(
						argument->type, parameter->current_type,
						concept->base.source_position, false)) {
				match = false;
				break;
			}
			
			argument  = argument->next;
			parameter = parameter->next;
		}
		if (match && (argument != NULL || parameter != NULL)) {
			print_error_prefix(instance->source_position);
			panic("type argument count of concept instance doesn't match "
			      "type parameter count of concept");
		}
		if (match)
			break;
	}

	return instance;
}

concept_instance_t *find_concept_instance(concept_t *concept)
{
	return _find_concept_instance(concept, NULL);
}

/** tests whether a type variable has a concept as constraint */
static bool type_variable_has_constraint(const type_variable_t *type_variable,
                                         const concept_t *concept)
{
	type_constraint_t *constraint = type_variable->constraints;
	while (constraint != NULL) {
		if (constraint->concept == concept)
			return true;

		constraint = constraint->next;
	}

	return false;
}

concept_function_instance_t *get_function_from_concept_instance(
		concept_instance_t *instance, concept_function_t *function)
{
	concept_function_instance_t *function_instance
		= instance->function_instances;
	while (function_instance != NULL) {
		if (function_instance->concept_function == function) {
			return function_instance;
		}

		function_instance = function_instance->next;
	}

	return NULL;
}

static void resolve_concept_function_instance(reference_expression_t *reference)
{
	entity_t *entity = reference->entity;
	assert(entity->kind == ENTITY_CONCEPT_FUNCTION);

	concept_function_t *concept_function = &entity->concept_function;
	concept_t          *concept          = concept_function->concept;

	/* test whether 1 of the type variables points to another type variable.
	 * this can happen when concept functions are invoked inside polymorphic
	 * functions. We can't resolve the function right now, but we have to check
	 * the constraints of the type variable */
	bool cant_resolve = false;
	type_variable_t *type_var = concept->type_parameters;
	while (type_var != NULL) {
		type_t *current_type = type_var->current_type;
		if (current_type == NULL)
			return;

		if (current_type->kind == TYPE_REFERENCE_TYPE_VARIABLE) {
			type_reference_t *type_ref      = (type_reference_t*) current_type;
			type_variable_t  *type_variable = type_ref->type_variable;

			if (!type_variable_has_constraint(type_variable, concept)) {
				print_error_prefix(reference->base.source_position);
				fprintf(stderr, "type variable '%s' needs a constraint for "
				        "concept '%s' when using function '%s'.\n",
				        type_variable->base.symbol->string,
				        concept->base.symbol->string,
				        concept_function->base.symbol->string);
				return;
			}
			cant_resolve = true;
		}

		type_var = type_var->next;
	}
	/* we have to defer the resolving for the ast2firm phase */
	if (cant_resolve) {
		return;
	}

	/* we assume that all typevars have current_type set */
	const source_position_t *pos      = &reference->base.source_position;
	concept_instance_t      *instance = _find_concept_instance(concept, pos);
	if (instance == NULL) {
		print_error_prefix(reference->base.source_position);
		fprintf(stderr, "there's no instance of concept '%s' for type ",
		        concept->base.symbol->string);
		type_variable_t *typevar = concept->type_parameters;
		while (typevar != NULL) {
			if (typevar->current_type != NULL) {
				print_type(typevar->current_type);
				fprintf(stderr, " ");
			}
			typevar = typevar->next;
		}
		fprintf(stderr, "\n");
		return;
	}

#if 0
	concept_function_instance_t *function_instance 
		= get_function_from_concept_instance(instance, concept_function);
	if (function_instance == NULL) {
		print_error_prefix(reference->base.source_position);
		fprintf(stderr, "no instance of function '%s' found in concept "
		        "instance?\n", concept_function->declaration.symbol->string);
		panic("panic");
	}

	type_t *type         = (type_t*) function_instance->function.type;
	type_t *pointer_type = make_pointer_type(type);

	reference->base.type   = pointer_type;
	reference->declaration = (declaration_t*) &function_instance->function;
#endif
}

static void check_type_constraints(type_variable_t *type_variables,
                                   const source_position_t source_position)
{
	type_variable_t *type_var     = type_variables;
	while (type_var != NULL) {
		type_constraint_t *constraint   = type_var->constraints;
		type_t            *current_type = type_var->current_type;

		for ( ;constraint != NULL; constraint = constraint->next) {
			concept_t *concept = constraint->concept;

			if (concept == NULL)
				continue;

			if (current_type->kind == TYPE_REFERENCE_TYPE_VARIABLE) {
				type_reference_t *ref      = (type_reference_t*) current_type;
				type_variable_t  *type_var = ref->type_variable;

				if (!type_variable_has_constraint(type_var, concept)) {
					print_error_prefix(source_position);
					fprintf(stderr, "type variable '%s' needs constraint "
					        "'%s'\n", type_var->base.symbol->string,
					        concept->base.symbol->string);
				}
				continue;
			}

			/* set typevariable values for the concept
			 * This currently only works for conceptes with 1 parameter */
			concept->type_parameters->current_type = type_var->current_type;
		
			concept_instance_t *instance 
				= _find_concept_instance(concept, & source_position);
			if (instance == NULL) {
				print_error_prefix(source_position);
				fprintf(stderr, "concrete type for type variable '%s' of "
				        "function doesn't match type constraints:\n",
				        type_var->base.symbol->string);
				print_error_prefix(source_position);
				fprintf(stderr, "type ");
				print_type(type_var->current_type);
				fprintf(stderr, " is no instance of concept '%s'\n",
				        concept->base.symbol->string);
			}

			/* reset typevar binding */
			concept->type_parameters->current_type = NULL;
		}

		type_var = type_var->next;
	}
}

/**
 * For variable argument functions, the last arguments are promoted as in the
 * C language: all integer types get INT, all pointers to void*, float becomes
 * double */
static type_t *get_default_param_type(type_t *type,
                                      source_position_t source_position)
{
	atomic_type_t *atomic_type;

	if (type == NULL) {
		return type_int;
	}

	type = skip_typeref(type);

	switch (type->kind) {
	case TYPE_ATOMIC:
		atomic_type = &type->atomic;
		switch (atomic_type->akind) {
		case ATOMIC_TYPE_INVALID:
			print_error_prefix(source_position);
			fprintf(stderr, "function argument has invalid type.\n");
			return error_type;

		case ATOMIC_TYPE_BOOL:
			return type_uint;

		case ATOMIC_TYPE_BYTE:
		case ATOMIC_TYPE_UBYTE:
		case ATOMIC_TYPE_INT:
		case ATOMIC_TYPE_UINT:
		case ATOMIC_TYPE_SHORT:
		case ATOMIC_TYPE_USHORT:
		case ATOMIC_TYPE_LONG:
		case ATOMIC_TYPE_ULONG:
		case ATOMIC_TYPE_LONGLONG:
		case ATOMIC_TYPE_ULONGLONG:
			return type_int;

		case ATOMIC_TYPE_FLOAT:
		case ATOMIC_TYPE_DOUBLE:
			return type_double;
		}
		break;

	case TYPE_ARRAY:
	case TYPE_POINTER:
		return type_void_ptr;

	case TYPE_FUNCTION:
		print_error_prefix(source_position);
		fprintf(stderr, "function type (");
		print_type(type);
		fprintf(stderr, ") not supported for function parameters.\n");
		return error_type;

	case TYPE_BIND_TYPEVARIABLES:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		print_error_prefix(source_position);
		fprintf(stderr, "compound type (");
		print_type(type);
		fprintf(stderr, ") not supported for function parameter.\n");
		return error_type;

	case TYPE_ERROR:
		return type;

	case TYPE_REFERENCE:
	case TYPE_REFERENCE_TYPE_VARIABLE:
	case TYPE_TYPEOF:
	case TYPE_VOID:
	case TYPE_INVALID:
		print_error_prefix(source_position);
		fprintf(stderr, "function argument has invalid type ");
		print_type(type);
		fprintf(stderr, "\n");
		return error_type;
	}
	print_error_prefix(source_position);
	panic("invalid type for function argument");
}

static void check_call_expression(call_expression_t *call)
{
	call->function                  = check_expression(call->function);
	expression_t    *function       = call->function;
	type_t          *type           = function->base.type;
	type_argument_t *type_arguments = NULL;

	/* can happen if we had a deeper semantic error */
	if (type == NULL)
		return;

	/* determine function type */
	if (type->kind != TYPE_POINTER) {
		print_error_prefix(call->base.source_position);
		fprintf(stderr, "trying to call non-pointer type ");
		print_type(type);
		fprintf(stderr, "\n");
		return;
	}
	pointer_type_t *pointer_type = (pointer_type_t*) type;

	type = pointer_type->points_to;
	if (type->kind != TYPE_FUNCTION) {
		print_error_prefix(call->base.source_position);
		fprintf(stderr, "trying to call a non-function value of type");
		print_type(type);
		fprintf(stderr, "\n");
		return;
	}
	function_type_t *function_type = (function_type_t*) type;

	/* match parameter types against type variables */
	type_variable_t *type_variables = NULL;
	if (function->kind == EXPR_REFERENCE) {
		reference_expression_t *reference
			= (reference_expression_t*) function;
		entity_t *entity = reference->entity;

		if (entity->kind == ENTITY_CONCEPT_FUNCTION) {
			concept_function_t *concept_function
				= &entity->concept_function;
			concept_t *concept = concept_function->concept;
		
			type_variables = concept->type_parameters;
			type_arguments = reference->type_arguments;
		} else if (entity->kind == ENTITY_FUNCTION) {
			function_entity_t *function_entity
				= &entity->function;

			type_variables = function_entity->function.type_parameters;
			type_arguments = reference->type_arguments;
		}
	}

	/* clear typevariable configuration */
	if (type_variables != NULL) {
		type_variable_t *type_var = type_variables;
		while (type_var != NULL) {
			type_var->current_type = NULL;

			type_var = type_var->next;
		}
	}

	/* apply type arguments */
	if (type_arguments != NULL) {
		type_variable_t *type_var      = type_variables;
		type_argument_t *type_argument = type_arguments;
		while (type_argument != NULL && type_var != NULL) {
			type_var->current_type = type_argument->type;

			type_var      = type_var->next;
			type_argument = type_argument->next;
		}

		if (type_argument != NULL || type_var != NULL) {
			error_at(function->base.source_position,
			         "wrong number of type arguments on function reference");
		}
	}

	/* check call arguments, match argument types against expected types
	 * and try to determine type variable configuration */
	call_argument_t           *argument   = call->arguments;
	function_parameter_type_t *param_type = function_type->parameter_types;
	int                      i          = 0;
	while (argument != NULL) {
		if (param_type == NULL && !function_type->variable_arguments) {
			error_at(call->base.source_position,
			         "too much arguments for function call\n");
			break;
		}

		argument->expression     = check_expression(argument->expression);
		expression_t *expression = argument->expression;

		type_t       *wanted_type;
		type_t       *expression_type = expression->base.type;

		if (param_type != NULL) {
			wanted_type = param_type->type;
		} else {
			wanted_type = get_default_param_type(expression_type,
			                        argument->expression->base.source_position);
		}

		/* match type of argument against type variables */
		if (type_variables != NULL && type_arguments == NULL) {
			match_variant_to_concrete_type(wanted_type, expression_type,
			                               expression->base.source_position,
										   true);
		} else if (expression_type != wanted_type) {
			/* be a bit lenient for varargs function, to not make using
			   C printf too much of a pain... */
			bool lenient = param_type == NULL;
			expression_t *new_expression 
				= make_cast(expression, wanted_type,
			                expression->base.source_position, lenient);
			if (new_expression == NULL) {
				print_error_prefix(expression->base.source_position);
				fprintf(stderr, "invalid type for argument %d of call: ", i);
				print_type(expression->base.type);
				fprintf(stderr, " should be ");
				print_type(wanted_type);
				fprintf(stderr, "\n");
			} else {
				expression = new_expression;
			}
		}
		argument->expression = expression;

		argument   = argument->next;
		if (param_type != NULL)
			param_type = param_type->next;
		++i;
	}
	if (param_type != NULL) {
		error_at(call->base.source_position,
		         "too few arguments for function call\n");
	}

	/* test whether we could determine the concrete types for all type
	 * variables */
	type_variable_t *type_var = type_variables;
	while (type_var != NULL) {
		if (type_var->current_type == NULL) {
			print_error_prefix(call->base.source_position);
			fprintf(stderr, "Couldn't determine concrete type for type "
					"variable '%s' in call expression\n",
			        type_var->base.symbol->string);
		}
#ifdef DEBUG_TYPEVAR_BINDING
		fprintf(stderr, "TypeVar '%s'(%p) bound to ",
		        type_var->base.symbol->string, type_var);
		print_type(type_var->current_type);
		fprintf(stderr, "\n");
#endif

		type_var = type_var->next;
	}
	
	/* normalize result type, as we know the concrete types for the typevars */
	type_t *result_type = function_type->result_type;
	if (type_variables != NULL) {
		reference_expression_t *ref    = (reference_expression_t*) function;
		entity_t               *entity = ref->entity;
		type_variable_t        *type_parameters;

		result_type = create_concrete_type(result_type);

		if (entity->kind == ENTITY_CONCEPT_FUNCTION) {
			/* we might be able to resolve the concept_function_instance now */
			resolve_concept_function_instance(ref);

			concept_function_t *concept_function = &entity->concept_function;
			concept_t          *concept          = concept_function->concept;
			type_parameters                      = concept->type_parameters;
		} else {
			/* check type constraints */
			assert(entity->kind == ENTITY_FUNCTION);
			check_type_constraints(type_variables,
			                       call->base.source_position);

			function_entity_t *function_entity = &entity->function;
			type_parameters = function_entity->function.type_parameters;
		}

		/* set type arguments on the reference expression */
		if (ref->type_arguments == NULL) {
			type_variable_t *type_var      = type_parameters;
			type_argument_t *last_argument = NULL;
			while (type_var != NULL) {
				type_argument_t *argument = allocate_ast(sizeof(argument[0]));
				memset(argument, 0, sizeof(argument[0]));

				type_t *current_type = type_var->current_type;
				argument->type       = current_type;

				if (last_argument != NULL) {
					last_argument->next = argument;
				} else {
					ref->type_arguments = argument;
				}
				last_argument = argument;
				type_var      = type_var->next;
			}
		}

		ref->base.type = create_concrete_type(ref->base.type);
	}

	/* clear typevariable configuration */
	if (type_variables != NULL) {
		type_variable_t *type_var = type_variables;
		while (type_var != NULL) {
			type_var->current_type = NULL;

#ifdef DEBUG_TYPEVAR_BINDINGS
			fprintf(stderr, "Unbind %s(%p)\n",
			        type_var->declaration.symbol->string, type_var);
#endif

			type_var = type_var->next;
		}
	}

	call->base.type = result_type;
}

static void check_cast_expression(unary_expression_t *cast)
{
	if (cast->base.type == NULL) {
		panic("Cast expression needs a datatype!");
	}
	cast->base.type = normalize_type(cast->base.type);
	cast->value     = check_expression(cast->value);

	if (cast->value->base.type == type_void) {
		error_at(cast->base.source_position,
		         "can't cast void type to anything\n");
	}
}

static void check_dereference_expression(unary_expression_t *dereference)
{
	dereference->value  = check_expression(dereference->value);
	expression_t *value = dereference->value;

	if (value->base.type == NULL) {
		error_at(dereference->base.source_position,
		         "can't derefence expression with unknown datatype\n");
		return;
	}
	if (value->base.type->kind != TYPE_POINTER) {
		error_at(dereference->base.source_position,
		         "can only dereference expressions with pointer type\n");
		return;
	}
	pointer_type_t *pointer_type      = (pointer_type_t*) value->base.type;
	type_t         *dereferenced_type = pointer_type->points_to;
	dereference->base.type            = dereferenced_type;
}

static void check_take_address_expression(unary_expression_t *expression)
{
	expression->value   = check_expression(expression->value);
	type_t *type        = expression->value->base.type;
	type_t *result_type = make_pointer_type(type);
	expression_t *value = expression->value;

	if (!is_lvalue(value)) {
		/* TODO use another word than lvalue to explain this to the user... */
		error_at(expression->base.source_position,
		         "can only take address of l-values\n");
		return;
	}

	if (value->kind == EXPR_REFERENCE) {
		reference_expression_t *reference = &value->reference;
		entity_t               *entity    = reference->entity;

		if (entity->kind == ENTITY_VARIABLE) {
			variable_t *variable = &entity->variable;
			variable->needs_entity = true;
		}
	}

	expression->base.type = result_type;
}

static bool is_arithmetic_type(type_t *type)
{
	if (type->kind != TYPE_ATOMIC)
		return false;

	atomic_type_t *atomic_type = (atomic_type_t*) type;

	switch (atomic_type->akind) {
	case ATOMIC_TYPE_BYTE:
	case ATOMIC_TYPE_UBYTE:
	case ATOMIC_TYPE_INT:
	case ATOMIC_TYPE_UINT:
	case ATOMIC_TYPE_SHORT:
	case ATOMIC_TYPE_USHORT:
	case ATOMIC_TYPE_LONG:
	case ATOMIC_TYPE_ULONG:
	case ATOMIC_TYPE_LONGLONG:
	case ATOMIC_TYPE_ULONGLONG:
	case ATOMIC_TYPE_FLOAT:
	case ATOMIC_TYPE_DOUBLE:
		return true;

	case ATOMIC_TYPE_INVALID:
	case ATOMIC_TYPE_BOOL:
		return false;
	}

	return false;
}

static void check_negate_expression(unary_expression_t *expression)
{
	expression->value = check_expression(expression->value);

	type_t *type = expression->value->base.type;
	if (type == NULL)
		return;

	if (!is_arithmetic_type(type)) {
		print_error_prefix(expression->base.source_position);
		fprintf(stderr, "negate expression only valid for arithmetic types, "
		        "but argument has type ");
		print_type(type);
		fprintf(stderr, "\n");
	}

	expression->base.type = type;
}

static void check_bitwise_not_expression(unary_expression_t *expression)
{
	expression->value = check_expression(expression->value);

	type_t *type = expression->value->base.type;
	if (type == NULL)
		return;

	if (!is_type_int(type)) {
		print_error_prefix(expression->base.source_position);
		fprintf(stderr, "not expression only valid for integer types, "
		        "but argument has type ");
		print_type(type);
		fprintf(stderr, "\n");
	}

	expression->base.type = type;
}

static expression_t *lower_incdec_expression(expression_t *expression_)
{
	unary_expression_t *expression = (unary_expression_t*) expression_;

	expression_t *value = check_expression(expression->value);
	type_t       *type  = value->base.type;

	expression_kind_t kind = expression->base.kind;

	if (!is_type_numeric(type) && type->kind != TYPE_POINTER) {
		print_error_prefix(expression->base.source_position);
		fprintf(stderr, "%s expression only valid for numeric or pointer types "
				"but argument has type ",
		        kind == EXPR_UNARY_INCREMENT ? "increment" : "decrement"
		        );
		print_type(type);
		fprintf(stderr, "\n");
	}
	if (!is_lvalue(value)) {
		print_error_prefix(expression->base.source_position);
		fprintf(stderr, "%s expression needs an lvalue\n",
	            kind == EXPR_UNARY_INCREMENT ? "increment" : "decrement"
		       );
	}

	bool need_int_const = true;
	if (type->kind == TYPE_ATOMIC) {
		atomic_type_t *atomic_type = (atomic_type_t*) type;
		if (atomic_type->akind == ATOMIC_TYPE_FLOAT ||
				atomic_type->akind == ATOMIC_TYPE_DOUBLE) {
			need_int_const = false;
		}
	}

	expression_t *constant;
	if (need_int_const) {
		constant                  = allocate_expression(EXPR_INT_CONST);
		constant->base.type       = type;
		constant->int_const.value = 1;
	} else {
		constant                    = allocate_expression(EXPR_FLOAT_CONST);
		constant->base.type         = type;
		constant->float_const.value = 1.0;
	}

	expression_t *add = allocate_expression(kind == EXPR_UNARY_INCREMENT
		                                    ? EXPR_BINARY_ADD
							                : EXPR_BINARY_SUB);
	add->base.type    = type;
	add->binary.left  = value;
	add->binary.right = constant;

	expression_t *assign = allocate_expression(EXPR_BINARY_ASSIGN);
	assign->base.type    = type;
	assign->binary.left  = value;
	assign->binary.right = add;

	return assign;
}

static void check_not_expression(unary_expression_t *expression)
{
	expression->value = check_expression(expression->value);

	type_t *type = expression->value->base.type;

	if (type != type_bool) {
		print_error_prefix(expression->base.source_position);
		fprintf(stderr, "not expression only valid for bool type, "
		        "but argument has type ");
		print_type(type);
		fprintf(stderr, "\n");
	}

	expression->base.type = type;
}

static void check_unary_expression(unary_expression_t *unary_expression)
{
	switch (unary_expression->base.kind) {
	case EXPR_UNARY_CAST:
		check_cast_expression(unary_expression);
		return;
	case EXPR_UNARY_DEREFERENCE:
		check_dereference_expression(unary_expression);
		return;
	case EXPR_UNARY_TAKE_ADDRESS:
		check_take_address_expression(unary_expression);
		return;
	case EXPR_UNARY_NOT:
		check_not_expression(unary_expression);
		return;
	case EXPR_UNARY_BITWISE_NOT:
		check_bitwise_not_expression(unary_expression);
		return;
	case EXPR_UNARY_NEGATE:
		check_negate_expression(unary_expression);
		return;
	case EXPR_UNARY_INCREMENT:
	case EXPR_UNARY_DECREMENT:
		panic("increment/decrement not lowered");
	default:
		break;
	}
	panic("Unknown unary expression found");
}

static entity_t *find_entity(const context_t *context, symbol_t *symbol)
{
	entity_t *entity = context->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		if (entity->base.symbol == symbol)
			break;
	}
	return entity;
}

static void check_select_expression(select_expression_t *select)
{
	select->compound       = check_expression(select->compound);
	expression_t *compound = select->compound;

	type_t *datatype = compound->base.type;
	if (datatype == NULL)
		return;

	bind_typevariables_type_t *bind_typevariables = NULL;
	compound_type_t           *compound_type;

	if (datatype->kind == TYPE_BIND_TYPEVARIABLES) {
		bind_typevariables = (bind_typevariables_type_t*) datatype;
		compound_type
			= (compound_type_t*) bind_typevariables->polymorphic_type;
	} else if (datatype->kind == TYPE_COMPOUND_STRUCT
			|| datatype->kind == TYPE_COMPOUND_UNION) {
		compound_type = (compound_type_t*) datatype;
	} else {
		if (datatype->kind != TYPE_POINTER) {
			print_error_prefix(select->base.source_position);
			fprintf(stderr, "select needs a compound type (or pointer) but "
					"found type ");
			print_type(datatype);
			fprintf(stderr, "\n");
			return;
		}

		pointer_type_t *pointer_type = (pointer_type_t*) datatype;
		
		type_t *points_to = pointer_type->points_to;
		if (points_to->kind == TYPE_BIND_TYPEVARIABLES) {
			bind_typevariables = (bind_typevariables_type_t*) points_to;
			compound_type
				= (compound_type_t*) bind_typevariables->polymorphic_type;
		} else if (points_to->kind == TYPE_COMPOUND_STRUCT
				|| points_to->kind == TYPE_COMPOUND_UNION) {
			compound_type = (compound_type_t*) points_to;
		} else {
			print_error_prefix(select->base.source_position);
			fprintf(stderr, "select needs a pointer to compound type but found "
					"type ");
			print_type(datatype);
			fprintf(stderr, "\n");
			return;
		}
	}

	symbol_t *symbol = select->symbol;

	/* try to find a matching declaration */
	entity_t *entity = find_entity(&compound_type->context, symbol);
	if (entity != NULL) {
		type_t *type = check_reference(entity, select->base.source_position);
		select->base.type = type;
		select->entity    = entity;
		return;
	}

	compound_entry_t *entry  = compound_type->entries;
	while (entry != NULL) {
		if (entry->symbol == symbol) {
			break;
		}
		entry = entry->next;
	}
	if (entry == NULL) {
		print_error_prefix(select->base.source_position);
		fprintf(stderr, "compound type ");
		print_type((type_t*) compound_type);
		fprintf(stderr, " does not have a member '%s'\n", symbol->string);
		return;
	}

	type_t *result_type = entry->type;

	/* resolve type varible bindings if needed */
	if (bind_typevariables != NULL) {
		int old_top = typevar_binding_stack_top();
		push_type_variable_bindings(compound_type->type_parameters,
		                            bind_typevariables->type_arguments);
		result_type = create_concrete_type(entry->type);
		pop_type_variable_bindings(old_top);
	}

	select->compound_entry = entry;
	select->base.type      = result_type;
}

static void check_array_access_expression(array_access_expression_t *access)
{
	access->array_ref       = check_expression(access->array_ref);
	access->index           = check_expression(access->index);
	expression_t *array_ref = access->array_ref;
	expression_t *index     = access->index;

	type_t *type = array_ref->base.type;
	if (type == NULL || 
			(type->kind != TYPE_POINTER && type->kind != TYPE_ARRAY)) {
		print_error_prefix(access->base.source_position);
		fprintf(stderr, "expected pointer or array type for array access, "
		        "got ");
		print_type(type);
		fprintf(stderr, "\n");
		return;
	}

	type_t *result_type;
	if (type->kind == TYPE_POINTER) {
		pointer_type_t *pointer_type = (pointer_type_t*) type;
		result_type                  = pointer_type->points_to;
	} else {
		assert(type->kind == TYPE_ARRAY);
		array_type_t *array_type = (array_type_t*) type;
		result_type              = array_type->element_type;

		/* TODO We could issue a warning if we have a constant index expression
		 * that exceeds the array size
		 */
	}
	access->base.type = result_type;

	if (index->base.type == NULL || !is_type_int(index->base.type)) {
		print_error_prefix(access->base.source_position);
		fprintf(stderr, "expected integer type for array index, got ");
		print_type(index->base.type);
		fprintf(stderr, "\n");
		return;
	}

	if (index->base.type != NULL && index->base.type != type_int) {
		access->index = make_cast(index, type_int,
		                          access->base.source_position, false);
	}
}

static void check_sizeof_expression(sizeof_expression_t *expression)
{
	expression->type      = normalize_type(expression->type);
	expression->base.type = type_uint;
}

static void check_func_expression(func_expression_t *expression)
{
	function_t *function = & expression->function;
	resolve_function_types(function);
	check_function(function, NULL, expression->base.source_position);

	expression->base.type = make_pointer_type((type_t*) function->type);
}

WARN_UNUSED
expression_t *check_expression(expression_t *expression)
{
	if (expression == NULL)
		return NULL;

	/* try to lower the expression */
	if ((unsigned) expression->kind < (unsigned) ARR_LEN(expression_lowerers)) {
		lower_expression_function lowerer 
			= expression_lowerers[expression->kind];

		if (lowerer != NULL && !expression->base.lowered) {
			expression = lowerer(expression);
		}
	}

	switch (expression->kind) {
	case EXPR_INT_CONST:
		expression->base.type = type_int;
		break;
	case EXPR_FLOAT_CONST:
		expression->base.type = type_double;
		break;
	case EXPR_BOOL_CONST:
		expression->base.type = type_bool;
		break;
	case EXPR_STRING_CONST:
		expression->base.type = type_byte_ptr;
		break;
	case EXPR_NULL_POINTER:
		expression->base.type = type_void_ptr;
		break;
	case EXPR_FUNC:
		check_func_expression((func_expression_t*) expression);
		break;

	case EXPR_REFERENCE:
		check_reference_expression((reference_expression_t*) expression);
		break;
	case EXPR_SIZEOF:
		check_sizeof_expression((sizeof_expression_t*) expression);
		break;
	EXPR_BINARY_CASES
		check_binary_expression((binary_expression_t*) expression);
		break;
	EXPR_UNARY_CASES
		check_unary_expression((unary_expression_t*) expression);
		break;
	case EXPR_SELECT:
		check_select_expression((select_expression_t*) expression);
		break;
	case EXPR_CALL:
		check_call_expression((call_expression_t*) expression);
		break;
	case EXPR_ARRAY_ACCESS:
		check_array_access_expression((array_access_expression_t*) expression);
		break;
	case EXPR_ERROR:
		found_errors = true;
		break;
	case EXPR_INVALID:
		panic("Invalid expression encountered");
	}

	return expression;
}




static void check_return_statement(return_statement_t *statement)
{
	function_t *function             = current_function;
	type_t     *function_result_type = function->type->result_type;
	statement->value                 = check_expression(statement->value);
	expression_t *return_value       = statement->value;

	last_statement_was_return = true;

	if (return_value != NULL) {
		if (function_result_type == type_void
				&& return_value->base.type != type_void) {
			error_at(statement->base.source_position,
			         "return with value in void function\n");
			return;
		}

		/* do we need a cast ?*/
		if (return_value->base.type != function_result_type) {
			return_value
				= make_cast(return_value, function_result_type,
				            statement->base.source_position, false);

			statement->value = return_value;
		}
	} else {
		if (function_result_type != type_void) {
			error_at(statement->base.source_position,
			         "missing return value in non-void function\n");
			return;
		}
	}
}

static void check_if_statement(if_statement_t *statement)
{
	statement->condition    = check_expression(statement->condition);
	expression_t *condition = statement->condition;

	assert(condition != NULL);
	if (condition->base.type != type_bool) {
		error_at(statement->base.source_position,
		         "if condition needs to be boolean but has type ");
		print_type(condition->base.type);
		fprintf(stderr, "\n");
		return;
	}

	statement->true_statement = check_statement(statement->true_statement);
	if (statement->false_statement != NULL) {
		statement->false_statement =
			check_statement(statement->false_statement);
	}
}

static void push_context(const context_t *context)
{
	entity_t *entity = context->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		environment_push(entity, context);
	}
}

static void check_block_statement(block_statement_t *block)
{
	int old_top = environment_top();

	check_and_push_context(& block->context);

	statement_t *statement = block->statements;
	statement_t *last      = NULL;
	while (statement != NULL) {
		statement_t *next = statement->base.next;

		statement = check_statement(statement);
		assert(statement->base.next == next || statement->base.next == NULL);
		statement->base.next = next;

		if (last != NULL) {
			last->base.next   = statement;
		} else {
			block->statements = statement;
		}

		last      = statement;
		statement = next;
	}

	environment_pop_to(old_top);
}

static void check_variable_declaration(declaration_statement_t *statement)
{
	function_t *function = current_function;
	variable_t *variable = &statement->entity;
	assert(function != NULL);

	variable->value_number = function->n_local_vars;
	function->n_local_vars++;

	/* TODO: try to catch cases where a variable is used before it is defined
	 * (Note: Adding the variable just here to the environment is not a good
	 *  idea the case were a variable is used earlier indicates an error
	 *  typically)
	 */

	variable->base.refs = 0;
	if (variable->type != NULL) {
		variable->type = normalize_type(variable->type);
	}
}

static void check_expression_statement(expression_statement_t *statement)
{
	statement->expression    = check_expression(statement->expression);
	expression_t *expression = statement->expression;

	/* can happen on semantic errors */
	if (expression->base.type == NULL)
		return;

	bool may_be_unused = false;
	if (expression->kind == EXPR_BINARY_ASSIGN) {
		may_be_unused = true;
	} else if (expression->kind == EXPR_UNARY_INCREMENT
			|| expression->kind == EXPR_UNARY_DECREMENT) {
		may_be_unused = true;
	} else if (expression->kind == EXPR_CALL) {
		may_be_unused = true;
	}

	if (expression->base.type != type_void && !may_be_unused) {
		print_warning_prefix(statement->base.source_position);
		fprintf(stderr, "result of expression is unused\n");
		if (expression->kind == EXPR_BINARY_EQUAL) {
			print_warning_prefix(statement->base.source_position);
			fprintf(stderr, "Did you mean '<-' instead of '='?\n");
		}
		print_warning_prefix(statement->base.source_position);
		fprintf(stderr, "note: cast expression to void to avoid this "
		        "warning\n");
	}
}

static void check_label_statement(label_statement_t *label)
{
	(void) label;
	/* nothing to do */
}

static void check_goto_statement(goto_statement_t *goto_statement)
{
	/* already resolved? */
	if (goto_statement->label != NULL)
		return;

	symbol_t *symbol = goto_statement->label_symbol;
	if (symbol == NULL) {
		error_at(goto_statement->base.source_position,
		         "unresolved anonymous goto\n");
		return;
	}

	entity_t *entity = symbol->entity;
	if (entity == NULL) {
		print_error_prefix(goto_statement->base.source_position);
		fprintf(stderr, "goto argument '%s' is an unknown symbol.\n",
		        symbol->string);
		return;
	}
	if (entity->kind != ENTITY_LABEL) {
		print_error_prefix(goto_statement->base.source_position);
		fprintf(stderr, "goto argument '%s' should be a label but is a '%s'.\n",
		        symbol->string, get_entity_kind_name(entity->kind));
		return;
	}

	label_t *label = &entity->label;
	goto_statement->label = label;
}

WARN_UNUSED
statement_t *check_statement(statement_t *statement)
{
	if (statement == NULL)
		return NULL;

	/* try to lower the statement */
	if ((int) statement->kind < (int) ARR_LEN(statement_lowerers)) {
		lower_statement_function lowerer = statement_lowerers[statement->kind];

		if (lowerer != NULL) {
			statement = lowerer(statement);
		}
	}

	if (statement == NULL)
		return NULL;

	last_statement_was_return = false;
	switch (statement->kind) {
	case STATEMENT_INVALID:
		panic("encountered invalid statement");
		break;
	case STATEMENT_BLOCK:
		check_block_statement(&statement->block);
		break;
	case STATEMENT_RETURN:
		check_return_statement(&statement->returns);
		break;
	case STATEMENT_GOTO:
		check_goto_statement(&statement->gotos);
		break;
	case STATEMENT_LABEL:
		check_label_statement(&statement->label);
		break;
	case STATEMENT_IF:
		check_if_statement(&statement->ifs);
		break;
	case STATEMENT_DECLARATION:
		check_variable_declaration(&statement->declaration);
		break;
	case STATEMENT_EXPRESSION:
		check_expression_statement(&statement->expression);
		break;
	default:
		panic("Unknown statement found");
		break;
	}

	return statement;
}

static void check_function(function_t *function, symbol_t *symbol,
                           const source_position_t source_position)
{
	if (function->is_extern)
		return;

	int old_top = environment_top();
	push_context(&function->context);

	function_t *last_function = current_function;
	current_function          = function;

	/* set function parameter numbers */
	function_parameter_t *parameter = function->parameters;
	int n = 0;
	while (parameter != NULL) {
		parameter->num = n;
		n++;
		parameter = parameter->next;
	}

	bool last_last_statement_was_return = last_statement_was_return;
	last_statement_was_return = false;
	if (function->statement != NULL) {
		function->statement = check_statement(function->statement);
	}

	if (!last_statement_was_return) {
		type_t *result_type = function->type->result_type;
		if (result_type != type_void) {
			/* TODO: report end-position of block-statement? */
			print_error_prefix(source_position);
			if (symbol != NULL) {
				fprintf(stderr, "missing return statement at end of function "
				        "'%s'\n", symbol->string);
			} else {
				fprintf(stderr, "missing return statement at end of anonymous "
				        "function\n");
			}
			return;
		}
	}

	current_function          = last_function;
	last_statement_was_return = last_last_statement_was_return;

	environment_pop_to(old_top);
}

static void check_constant(constant_t *constant)
{
	expression_t *expression = constant->expression;

	expression = check_expression(expression);
	if (expression->base.type != constant->type) {
		expression = make_cast(expression, constant->type,
		                       constant->base.source_position, false);
	}
	constant->expression = expression;

	if (!is_constant_expression(expression)) {
		print_error_prefix(constant->base.source_position);
		fprintf(stderr, "Value for constant '%s' is not constant\n",
		        constant->base.symbol->string);
	}
}

static void resolve_type_constraint(type_constraint_t *constraint,
                                    const source_position_t source_position)
{
	symbol_t *symbol = constraint->concept_symbol;
	entity_t *entity = symbol->entity;

	if (entity == NULL) {
		print_error_prefix(source_position);
		fprintf(stderr, "nothing known about symbol '%s'\n", symbol->string);
		return;
	}
	if (entity->kind != ENTITY_CONCEPT) {
		print_error_prefix(source_position);
		fprintf(stderr, "expected a concept but symbol '%s' is a '%s'\n",
		        symbol->string, get_entity_kind_name(entity->kind));
		return;
	}

	constraint->concept = &entity->concept;
}

static void resolve_type_variable_constraints(type_variable_t *type_variables)
{
	type_variable_t *type_var = type_variables;
	while (type_var != NULL) {
		type_constraint_t *constraint = type_var->constraints;

		for ( ; constraint != NULL; constraint = constraint->next) {
			resolve_type_constraint(constraint, type_var->base.source_position);
		}
		type_var = type_var->next;
	}
}

static void resolve_function_types(function_t *function)
{
	int old_top = environment_top();

	/* push type variables */
	push_context(&function->context);
	resolve_type_variable_constraints(function->type_parameters);

	/* normalize parameter types */
	function_parameter_t *parameter = function->parameters;
	for ( ; parameter != NULL; parameter = parameter->next) {
		parameter->type = normalize_type(parameter->type);
	}

	function->type = (function_type_t*) normalize_type((type_t*)function->type);

	environment_pop_to(old_top);
}

static void check_concept_instance(concept_instance_t *instance)
{
	concept_function_instance_t *function_instance 
		= instance->function_instances;
	while (function_instance != NULL) {
		function_t *function = &function_instance->function;
		resolve_function_types(function);
		check_function(function, function_instance->symbol,
		               function_instance->source_position);

		function_instance = function_instance->next;
	}
}

static void resolve_concept_types(concept_t *concept)
{
	int old_top = environment_top();

	/* push type variables */
	type_variable_t *type_parameter = concept->type_parameters;
	while (type_parameter != NULL) {
		entity_t *entity = (entity_t*) type_parameter;
		environment_push(entity, concept);

		type_parameter = type_parameter->next;
	}
	resolve_type_variable_constraints(concept->type_parameters);

	/* normalize function types */
	concept_function_t *concept_function = concept->functions;
	for (; concept_function!=NULL; concept_function = concept_function->next) {
		type_t *normalized_type 
			= normalize_type((type_t*) concept_function->type);
		assert(normalized_type->kind == TYPE_FUNCTION);
		concept_function->type = (function_type_t*) normalized_type;
	}

	environment_pop_to(old_top);
}


static void resolve_concept_instance(concept_instance_t *instance)
{
	symbol_t *symbol = instance->concept_symbol;
	entity_t *entity = symbol->entity;

	if (entity == NULL) {
		print_error_prefix(instance->source_position);
		fprintf(stderr, "symbol '%s' is unknown\n", symbol->string);
		return;
	}
	if (entity->kind != ENTITY_CONCEPT) {
		print_error_prefix(entity->base.source_position);
		fprintf(stderr, "expected a concept but symbol '%s' is a '%s'\n",
		        symbol->string, get_entity_kind_name(entity->kind));
		return;
	}

	concept_t *concept        = &entity->concept;
	instance->concept         = concept;
	instance->next_in_concept = concept->instances;
	concept->instances        = instance;

	int old_top = environment_top();

	/* push type variables */
	resolve_type_variable_constraints(instance->type_parameters);

	type_variable_t *type_parameter = instance->type_parameters;
	for ( ; type_parameter != NULL; type_parameter = type_parameter->next) {
		entity_t *entity = (entity_t*) type_parameter;
		environment_push(entity, instance);
	}

	/* normalize argument types */
	type_argument_t *type_argument = instance->type_arguments;
	while (type_argument != NULL) {
		type_argument->type = normalize_type(type_argument->type);

		type_argument = type_argument->next;
	}

	/* link functions and normalize their types */
	size_t n_concept_functions = 0;
	concept_function_t *function = concept->functions;
	for ( ; function != NULL; function = function->next) {
		++n_concept_functions;
	}
	bool have_function[n_concept_functions];
	memset(&have_function, 0, sizeof(have_function));

	concept_function_instance_t *function_instance
		= instance->function_instances;
	for (; function_instance != NULL; 
			function_instance = function_instance->next) {

		/* find corresponding concept function */
		int n = 0;
		for (function = concept->functions; function != NULL;
		     function = function->next, ++n) {
			if (function->base.symbol == function_instance->symbol)
				break;
		}

		if (function == NULL) {
			print_warning_prefix(function_instance->source_position);
			fprintf(stderr, "concept '%s' does not declare a function '%s'\n",
			        concept->base.symbol->string,
					function->base.symbol->string);
		} else {
			function_instance->concept_function = function;
			function_instance->concept_instance = instance;
			if (have_function[n]) {
				print_error_prefix(function_instance->source_position);
				fprintf(stderr,
				        "multiple implementations of function '%s' found in instance of concept '%s'\n",
						function->base.symbol->string,
						concept->base.symbol->string);
			}
			have_function[n] = true;
		}
		function_t *ifunction = & function_instance->function;

		if (ifunction->type_parameters != NULL) {
			print_error_prefix(function_instance->source_position);
			fprintf(stderr,
			        "instance function '%s' must not have type parameters\n",
					function_instance->symbol->string);
		}
		
		ifunction->type 
			= (function_type_t*) normalize_type((type_t*) ifunction->type);
	}

	size_t n = 0;
	for (function = concept->functions; function != NULL;
	     function = function->next, ++n) {
		if (!have_function[n]) {
			print_error_prefix(instance->source_position);
			fprintf(stderr, "instance of concept '%s' does not implement "
					"function '%s'\n", concept->base.symbol->string,
			        function->base.symbol->string);
		}
	}

	environment_pop_to(old_top);
}

static void check_export(const export_t *export)
{
	symbol_t *symbol = export->symbol;
	entity_t *entity = symbol->entity;

	if (entity == NULL) {
		print_error_prefix(export->source_position);
		fprintf(stderr, "Exported symbol '%s' is unknown\n", symbol->string);
		return;
	}

	entity->base.exported = true;
	found_export          = true;
}

static void check_and_push_context(context_t *context)
{
	push_context(context);

	/* normalize types, resolve concept instance references */
	entity_t *entity = context->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		switch (entity->kind) {
		case ENTITY_VARIABLE:
			entity->variable.type = normalize_type(entity->variable.type);
			break;
		case ENTITY_FUNCTION:
			resolve_function_types(&entity->function.function);
			break;
		case ENTITY_TYPEALIAS: {
			type_t *type = normalize_type(entity->typealias.type);
			if (type->kind == TYPE_COMPOUND_UNION
				|| type->kind == TYPE_COMPOUND_STRUCT) {
				check_compound_type((compound_type_t*) type);
			}
			entity->typealias.type = type;
			break;
		}
		case ENTITY_CONCEPT:
			resolve_concept_types(&entity->concept);
			break;
		default:
			break;
		}
	}

	concept_instance_t *instance = context->concept_instances;
	for ( ; instance != NULL; instance = instance->next) {
		resolve_concept_instance(instance);
	}
	
	/* check semantics in conceptes */
	instance = context->concept_instances;
	for ( ; instance != NULL; instance = instance->next) {
		check_concept_instance(instance);
	}

	/* check semantics in functions */
	entity = context->entities;
	for ( ; entity != NULL; entity = entity->base.next) {
		switch (entity->kind) {
		case ENTITY_FUNCTION: {
			check_function(&entity->function.function,
			               entity->base.symbol,
			               entity->base.source_position);
			break;
		}
		case ENTITY_CONSTANT:
			check_constant(&entity->constant);
			break;
		default:
			break;
		}
	}

	/* handle export declarations */
	export_t *export = context->exports;
	for ( ; export != NULL; export = export->next) {
		check_export(export);
	}
}

void register_statement_lowerer(lower_statement_function function,
                                unsigned int statement_type)
{
	unsigned int len = ARR_LEN(statement_lowerers);
	if (statement_type >= len) {
		ARR_RESIZE(lower_statement_function, statement_lowerers, statement_type + 1);
		memset(&statement_lowerers[len], 0,
		       (statement_type - len + 1) * sizeof(statement_lowerers[0]));
	}

	if (statement_lowerers[statement_type] != NULL) {
		panic("Trying to register multiple lowerers for a statement type");
	}
	statement_lowerers[statement_type] = function;
}

void register_expression_lowerer(lower_expression_function function,
                                 unsigned int expression_type)
{
	unsigned int len = ARR_LEN(expression_lowerers);
	if (expression_type >= len) {
		ARR_RESIZE(lower_expression_function, expression_lowerers, expression_type + 1);
		memset(&expression_lowerers[len], 0,
		       (expression_type - len + 1) * sizeof(expression_lowerers[0]));
	}

	if (expression_lowerers[expression_type] != NULL) {
		panic("Trying to register multiple lowerers for a expression type");
	}
	expression_lowerers[expression_type] = function;
}

static module_t *find_module(symbol_t *name)
{
	module_t *module = modules;
	for ( ; module != NULL; module = module->next) {
		if (module->name == name)
			break;
	}
	return module;
}

static void check_module(module_t *module)
{
	if (module->processed)
		return;
	assert(!module->processing);
	module->processing = true;

	int old_top = environment_top();

	/* check imports */
	import_t *import = module->context.imports;
	for( ; import != NULL; import = import->next) {
		const context_t *ref_context = NULL;
		entity_t        *entity;

		symbol_t *symbol     = import->symbol;
		symbol_t *modulename = import->module;
		module_t *ref_module = find_module(modulename);
		if (ref_module == NULL) {
			print_error_prefix(import->source_position);
			fprintf(stderr, "Referenced module \"%s\" does not exist\n",
			        modulename->string);
			entity = create_error_entity(symbol);
		} else {
			if (ref_module->processing) {
				print_error_prefix(import->source_position);
				fprintf(stderr, "Reference to module '%s' is recursive\n",
				        modulename->string);
				entity = create_error_entity(symbol);
			} else {
				check_module(ref_module);
				entity = find_entity(&ref_module->context, symbol);
				if (entity == NULL) {
					print_error_prefix(import->source_position);
					fprintf(stderr, "Module '%s' does not declare '%s'\n",
					        modulename->string, symbol->string);
					entity = create_error_entity(symbol);
				} else {
					ref_context = &ref_module->context;
				}
			}
		}
		if (!entity->base.exported) {
			print_error_prefix(import->source_position);
			fprintf(stderr, "Cannot import '%s' from \"%s\" because it is not exported\n",
			        symbol->string, modulename->string);
		}
		if (symbol->entity == entity) {
			print_warning_prefix(import->source_position);
			fprintf(stderr, "'%s' imported twice\n", symbol->string);
			/* imported twice, ignore */
			continue;
		}

		environment_push(entity, ref_context);
	}

	check_and_push_context(&module->context);
	environment_pop_to(old_top);

	assert(module->processing);
	module->processing = false;
	assert(!module->processed);
	module->processed = true;
}

bool check_semantic(void)
{
	obstack_init(&symbol_environment_obstack);

	symbol_stack  = NEW_ARR_F(environment_entry_t*, 0);
	found_errors  = false;
	found_export  = false;

	type_bool     = make_atomic_type(ATOMIC_TYPE_BOOL);
	type_byte     = make_atomic_type(ATOMIC_TYPE_BYTE);
	type_int      = make_atomic_type(ATOMIC_TYPE_INT);
	type_uint     = make_atomic_type(ATOMIC_TYPE_UINT);
	type_double   = make_atomic_type(ATOMIC_TYPE_DOUBLE);
	type_void_ptr = make_pointer_type(type_void);
	type_byte_ptr = make_pointer_type(type_byte);
	error_type    = type_void;

	module_t *module = modules;
	for ( ; module != NULL; module = module->next) {
		check_module(module);
	}

	if (!found_export) {
		fprintf(stderr, "error: no symbol exported\n");
		found_errors = true;
	}

	DEL_ARR_F(symbol_stack);
	obstack_free(&symbol_environment_obstack, NULL);

	return !found_errors;
}

void init_semantic_module(void)
{
	statement_lowerers  = NEW_ARR_F(lower_statement_function, 0);
	expression_lowerers = NEW_ARR_F(lower_expression_function, 0);

	register_expression_lowerer(lower_incdec_expression, EXPR_UNARY_INCREMENT);
	register_expression_lowerer(lower_incdec_expression, EXPR_UNARY_DECREMENT);
	register_expression_lowerer(lower_sub_expression, EXPR_BINARY_SUB);
}

void exit_semantic_module(void)
{
	DEL_ARR_F(expression_lowerers);
	DEL_ARR_F(statement_lowerers);
}

