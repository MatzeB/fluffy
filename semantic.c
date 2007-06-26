#include <config.h>

#include "semantic_t.h"

#include "ast_t.h"
#include "type_t.h"
#include "type_hash.h"
#include "match_type.h"
#include "adt/obst.h"
#include "adt/array.h"
#include "adt/error.h"

//#define DEBUG_TYPEVAR_BINDINGS

static lower_statement_function  *statement_lowerers  = NULL;
static lower_expression_function *expression_lowerers = NULL;

static struct obstack        symbol_environment_obstack;
static environment_entry_t **symbol_stack;
static struct obstack        label_obstack;
static symbol_t            **label_stack;
static int                   found_errors;

static type_t *type_bool     = NULL;
static type_t *type_byte     = NULL;
static type_t *type_int      = NULL;
static type_t *type_uint     = NULL;
static type_t *type_byte_ptr = NULL;
static type_t *type_void_ptr = NULL;

static method_t *current_method            = NULL;
int              last_statement_was_return = 0;

typedef enum environment_entry_type_t environment_entry_type_t;

enum environment_entry_type_t {
	ENTRY_INVALID,
	ENTRY_LOCAL_VARIABLE,
	ENTRY_GLOBAL_VARIABLE,
	ENTRY_CONSTANT,
	ENTRY_METHOD_PARAMETER,
	ENTRY_METHOD,
	ENTRY_TYPECLASS,
	ENTRY_TYPEALIAS,
	ENTRY_TYPE_VARIABLE,
	ENTRY_EXTERN_METHOD,
	ENTRY_GOTO,
	ENTRY_LABEL,
	ENTRY_TYPECLASS_METHOD
};

struct environment_entry_t {
	environment_entry_type_t  type;
	symbol_t                 *symbol;
	environment_entry_t      *up;
	union {
		method_t                         *method;
		global_variable_t                *global_variable;
		constant_t                       *constant;
		method_parameter_t               *method_parameter;
		type_t                           *stored_type;
		type_variable_t                  *type_variable;
		variable_declaration_statement_t *variable;
		goto_statement_t                 *goto_statement;
		label_statement_t                *label;
		typeclass_t                      *typeclass;
		typeclass_method_t               *typeclass_method;
	} e;
};

const char *get_entry_type_name(environment_entry_type_t type)
{
	switch(type) {
	case ENTRY_INVALID:          return "invalid reference";
	case ENTRY_LOCAL_VARIABLE:   return "local variable";
	case ENTRY_GLOBAL_VARIABLE:  return "global variable";
	case ENTRY_CONSTANT:         return "constant";
	case ENTRY_METHOD_PARAMETER: return "method parameter";
	case ENTRY_METHOD:           return "method";
	case ENTRY_TYPECLASS:        return "typeclass";
	case ENTRY_TYPEALIAS:        return "type alias";
	case ENTRY_TYPE_VARIABLE:    return "type variable";
	case ENTRY_EXTERN_METHOD:    return "extern method";
	case ENTRY_GOTO:             return "goto";
	case ENTRY_LABEL:            return "label";
	case ENTRY_TYPECLASS_METHOD: return "typeclass method";
	}
	panic("invalid environment entry found");
}

void print_error_prefix(const source_position_t position)
{
	fprintf(stderr, "%s:%d: error: ", position.input_name, position.linenr);
	found_errors = 1;
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
static inline
environment_entry_t *environment_push(symbol_t *symbol)
{
	environment_entry_t *entry 
		= obstack_alloc(&symbol_environment_obstack, sizeof(entry[0]));
	memset(entry, 0, sizeof(entry[0]));

	int top = ARR_LEN(symbol_stack);
	ARR_RESIZE(symbol_stack, top + 1);
	symbol_stack[top] = entry;

	entry->up     = symbol->thing;
	entry->symbol = symbol;
	symbol->thing = entry;

	return entry;
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

	if(new_top == top)
		return;

	assert(new_top < top);
	i = top;
	do {
		          entry  = symbol_stack[i - 1];
		symbol_t *symbol = entry->symbol;

		if(entry->type == ENTRY_LOCAL_VARIABLE
				&& entry->e.variable->refs == 0) {
			variable_declaration_statement_t *variable = entry->e.variable;
			print_warning_prefix(variable->statement.source_position);
			fprintf(stderr, "variable '%s' was declared but never read\n",
			        symbol->string);
		}

		assert(symbol->thing == entry);
		symbol->thing = entry->up;

		--i;
	} while(i != new_top);
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

static
type_t* make_atomic_type(atomic_type_type_t atype)
{
	atomic_type_t *type = obstack_alloc(type_obst, sizeof(type[0]));
	memset(type, 0, sizeof(type[0]));
	type->type.type = TYPE_ATOMIC;
	type->atype     = atype;

	type_t *normalized_type = typehash_insert((type_t*) type);
	if(normalized_type != (type_t*) type) {
		obstack_free(type_obst, type);
	}

	return normalized_type;
}

static
type_t* make_pointer_type(type_t *points_to)
{
	pointer_type_t *type = obstack_alloc(type_obst, sizeof(type[0]));
	memset(type, 0, sizeof(type[0]));
	type->type.type = TYPE_POINTER;
	type->points_to = points_to;

	type_t *normalized_type = typehash_insert((type_t*) type);
	if(normalized_type != (type_t*) type) {
		obstack_free(type_obst, type);
	}

	return normalized_type;
}

static
type_t *normalize_type(type_t *type);

static
type_t *resolve_type_reference(type_reference_t *type_ref)
{
	symbol_t            *symbol = type_ref->symbol;
	environment_entry_t *entry  = symbol->thing;
	if(entry == NULL) {
		print_error_prefix(type_ref->source_position);
		fprintf(stderr, "can't resolve type: symbol '%s' is unknown\n",
		        symbol->string);
		return NULL;
	}
	if(entry->type == ENTRY_TYPE_VARIABLE) {
		type_variable_t *type_variable = entry->e.type_variable;

		if(type_variable->current_type != NULL) {
			/* not sure if this is really a problem... */
			fprintf(stderr, "Debug warning: unresolved type var ref found "
			        "a concrete type...\n");
			return type_variable->current_type;
		}
		type_ref->type.type       = TYPE_REFERENCE_TYPE_VARIABLE;
		type_ref->r.type_variable = entry->e.type_variable;
		return typehash_insert((type_t*) type_ref);
	}	

	if(entry->type != ENTRY_TYPEALIAS) {
		print_error_prefix(type_ref->source_position);
		fprintf(stderr, "expected a type alias, but '%s' is a '%s'\n",
		        symbol->string, get_entry_type_name(entry->type));
		return NULL;
	}

	entry->e.stored_type = normalize_type(entry->e.stored_type);

	return entry->e.stored_type;
}

static
type_t *resolve_type_reference_type_var(type_reference_t *type_ref)
{
	type_variable_t *type_variable = type_ref->r.type_variable;
	if(type_variable->current_type != NULL) {
		return normalize_type(type_variable->current_type);
	}

	return typehash_insert((type_t*) type_ref);
}

static
type_t *normalize_pointer_type(pointer_type_t *type)
{
	type->points_to = normalize_type(type->points_to);

	return typehash_insert((type_t*) type);
}

static
type_t *normalize_array_type(array_type_t *type)
{
	type->element_type = normalize_type(type->element_type);

	return typehash_insert((type_t*) type);
}

static
type_t *normalize_method_type(method_type_t *method_type)
{
	method_type->result_type = normalize_type(method_type->result_type);

	method_parameter_type_t *parameter = method_type->parameter_types;
	while(parameter != NULL) {
		parameter->type = normalize_type(parameter->type);

		parameter = parameter->next;
	}

	return typehash_insert((type_t*) method_type);
}

static
void normalize_compound_entries(compound_type_t *type)
{
	compound_entry_t *entry = type->entries;
	while(entry != NULL) {
		entry->type = normalize_type(entry->type);

		entry = entry->next;
	}
}

static
type_t *normalize_compound_type(compound_type_t *type)
{
	return typehash_insert((type_t*) type);
}

static
type_t *normalize_type(type_t *type)
{
	/* happens sometimes on semantic errors */
	if(type == NULL)
		return NULL;

	switch(type->type) {
	case TYPE_INVALID:
	case TYPE_VOID:
	case TYPE_ATOMIC:
		return type;

	case TYPE_REFERENCE:
		return resolve_type_reference((type_reference_t*) type);

	case TYPE_REFERENCE_TYPE_VARIABLE:
		return resolve_type_reference_type_var((type_reference_t*) type);

	case TYPE_POINTER:
		return normalize_pointer_type((pointer_type_t*) type);

	case TYPE_ARRAY:
		return normalize_array_type((array_type_t*) type);
	
	case TYPE_METHOD:
		return normalize_method_type((method_type_t*) type);

	case TYPE_COMPOUND_UNION:
	case TYPE_COMPOUND_STRUCT:
		return normalize_compound_type((compound_type_t*) type);
	}

	panic("Unknown type found");
}



static
void check_local_variable_type(variable_declaration_statement_t *declaration,
                               type_t *type)
{
	if(type->type != TYPE_ATOMIC && type->type != TYPE_POINTER
			&& type->type != TYPE_COMPOUND_STRUCT
			&& type->type != TYPE_COMPOUND_UNION) {
		if(type->type == TYPE_REFERENCE_TYPE_VARIABLE) {
			/* TODO: we need to be able to handle all types in local vars... */
			type_reference_t *ref           = (type_reference_t*) type;
			type_variable_t  *type_variable = ref->r.type_variable;
			print_warning_prefix(declaration->statement.source_position);
			fprintf(stderr, "can't decide whether type variable '%s' is atomic "
			        "or pointer.\n", type_variable->symbol->string);
			return;
		}
		print_error_prefix(declaration->statement.source_position);
		fprintf(stderr, "only atomic or pointer types allowed for local "
		        "variables (at variable '%s')\n", declaration->symbol->string);
	}
}

static
void check_reference_expression(reference_expression_t *ref)
{
	variable_declaration_statement_t *variable;
	method_t                         *method;
	method_parameter_t               *method_parameter;
	global_variable_t                *global_variable;
	constant_t                       *constant;
	typeclass_method_t               *typeclass_method;
	symbol_t                         *symbol = ref->symbol;
	environment_entry_t              *entry  = symbol->thing;

	if(entry == NULL) {
		print_error_prefix(ref->expression.source_position);
		fprintf(stderr, "no known definition for '%s'\n", symbol->string);
		return;
	}

	/* resolve reference */
	switch(entry->type) {
	case ENTRY_LOCAL_VARIABLE:
		variable                 = entry->e.variable;
		ref->r.variable          = variable;
		ref->expression.type     = EXPR_REFERENCE_VARIABLE;
		ref->expression.datatype = variable->type;
		variable->refs++;
		break;
	case ENTRY_METHOD:
		method                   = entry->e.method;
		ref->r.method            = method;
		ref->expression.type     = EXPR_REFERENCE_METHOD;
		ref->expression.datatype = make_pointer_type((type_t*) method->type);
		break;
	case ENTRY_GLOBAL_VARIABLE:
		global_variable          = entry->e.global_variable;
		ref->r.global_variable   = global_variable;
		ref->expression.type     = EXPR_REFERENCE_GLOBAL_VARIABLE;
		ref->expression.datatype = global_variable->type;
		if(ref->expression.datatype->type == TYPE_COMPOUND_STRUCT
				|| ref->expression.datatype->type == TYPE_COMPOUND_UNION
				|| ref->expression.datatype->type == TYPE_ARRAY) {
			ref->expression.datatype = make_pointer_type(
					ref->expression.datatype);
		}
		break;
	case ENTRY_CONSTANT:
		constant                 = entry->e.constant;
		ref->r.constant          = constant;
		ref->expression.type     = EXPR_REFERENCE_CONSTANT;
		if(constant->type == NULL) {
			constant->expression = check_expression(constant->expression);
			constant->type       = constant->expression->datatype;
		}
		ref->expression.datatype = constant->type;
		break;
	case ENTRY_METHOD_PARAMETER:
		method_parameter         = entry->e.method_parameter;
		ref->r.method_parameter  = method_parameter;
		ref->expression.type     = EXPR_REFERENCE_METHOD_PARAMETER;
		ref->expression.datatype = method_parameter->type;
		break;
	case ENTRY_TYPECLASS_METHOD:
		typeclass_method         = entry->e.typeclass_method;
		ref->r.typeclass_method  = typeclass_method;
		ref->expression.type     = EXPR_REFERENCE_TYPECLASS_METHOD;
		ref->expression.datatype = make_pointer_type((type_t*) typeclass_method->method_type);
		break;
	case ENTRY_GOTO:
	case ENTRY_LABEL:
	case ENTRY_TYPEALIAS:
	case ENTRY_TYPECLASS:
	case ENTRY_TYPE_VARIABLE:
	case ENTRY_EXTERN_METHOD:
		print_error_prefix(ref->expression.source_position);
		fprintf(stderr, "'%s' is a '%s' and can't be used as expression\n",
		        ref->symbol->string, get_entry_type_name(entry->type));
		break;
	case ENTRY_INVALID:
		panic("Unknown reference type encountered");
		break;
	}

	/* normalize type arguments */
	type_argument_t *type_argument = ref->type_arguments;
	while(type_argument != NULL) {
		type_argument->type = normalize_type(type_argument->type);

		type_argument = type_argument->next;
	}
}

static
int is_lvalue(const expression_t *expression)
{
	unary_expression_t  *unexpr;
	select_expression_t *select;

	switch(expression->type) {
	case EXPR_REFERENCE_VARIABLE:
	case EXPR_REFERENCE_GLOBAL_VARIABLE:
	case EXPR_ARRAY_ACCESS:
		return 1;
	case EXPR_SELECT:
		select = (select_expression_t*) expression;
		type_t *entry_type = select->compound_entry->type;

		return entry_type->type != TYPE_COMPOUND_STRUCT &&
			entry_type->type != TYPE_COMPOUND_UNION &&
			entry_type->type != TYPE_ARRAY;
	case EXPR_UNARY:
		unexpr = (unary_expression_t*) expression;
		if(unexpr->type == UNEXPR_DEREFERENCE)
			return 1;
		break;
	default:
		break;
	}

	return 0;
}

static
void check_assign_expression(binary_expression_t *assign)
{
	expression_t *left  = assign->left;
	expression_t *right = assign->right;

	if(!is_lvalue(left)) {
		error_at(assign->expression.source_position,
		         "left side of assign is not an lvalue.\n");
		return;
	}
	if(left->type == EXPR_REFERENCE_VARIABLE) {
		reference_expression_t *ref = (reference_expression_t*) left;
		variable_declaration_statement_t *variable = ref->r.variable;
		symbol_t                         *symbol   = variable->symbol;

		/* do type inferencing if needed */
		if(left->datatype == NULL) {
			if(right->datatype == NULL) {
				print_error_prefix(assign->expression.source_position);
				fprintf(stderr, "can't infer type for '%s'\n", symbol->string);
				return;
			}

			variable->type = right->datatype;
			left->datatype = right->datatype;
#if 0
			fprintf(stderr, "Type inference for '%s': ", symbol->string);
			print_type(stderr, right->datatype);
			fputs("\n", stderr);
#endif

			check_local_variable_type(variable, right->datatype);
		}

		/* making an assignment is not reading the value */
		variable->refs--;
	}
}

static
expression_t *make_cast(expression_t *from,
                        type_t *dest_type,
                        const source_position_t source_position)
{
	assert(from->datatype != dest_type);
	if(dest_type == NULL)
		return from;

	/* TODO: - test which types can be implicitely casted... 
	 *       - improve error reporting (want to know the context of the cast)
	 *          ("can't implicitely cast for argument 2 of method call...")
	 */

	type_t *from_type = from->datatype;
	if(from_type == NULL) {
		print_error_prefix(from->source_position);
		fprintf(stderr, "can't implicitely cast from unknown type to ");
		print_type(stderr, dest_type);
		fprintf(stderr, "\n");
		return NULL;
	}

	int implicit_cast_allowed = 1;
	if(from_type->type == TYPE_POINTER) {
		if(dest_type->type == TYPE_POINTER) {
			pointer_type_t *p1 = (pointer_type_t*) from_type;
			pointer_type_t *p2 = (pointer_type_t*) dest_type;
			/* you can implicitely cast any pointer to void* and
			 * it is allowed to cast 'null' to any pointer */
			if(p1->points_to != p2->points_to
					&& dest_type != type_void_ptr
					&& from->type != EXPR_NULL_POINTER) {
				implicit_cast_allowed = 0;
			}
		} else {
			implicit_cast_allowed = 0;
		}
	} else if(dest_type->type == TYPE_POINTER) {
		implicit_cast_allowed = 0;
	} else if(from_type->type == TYPE_ATOMIC) {
		if(dest_type->type != TYPE_ATOMIC) {
			implicit_cast_allowed = 0;
		} else {
			atomic_type_t      *from_type_atomic = (atomic_type_t*) from_type;
			atomic_type_type_t  from_atype       = from_type_atomic->atype;
			atomic_type_t      *dest_type_atomic = (atomic_type_t*) dest_type;
			atomic_type_type_t  dest_atype       = dest_type_atomic->atype;

			switch(from_atype) {
			case ATOMIC_TYPE_UBYTE:
				implicit_cast_allowed |=
					(dest_atype == ATOMIC_TYPE_USHORT) ||
					(dest_atype == ATOMIC_TYPE_SHORT);
			case ATOMIC_TYPE_USHORT:
				implicit_cast_allowed |=
					(dest_atype == ATOMIC_TYPE_UINT) ||
					(dest_atype == ATOMIC_TYPE_INT);
			case ATOMIC_TYPE_UINT:
				implicit_cast_allowed |=
					(dest_atype == ATOMIC_TYPE_ULONG) ||
					(dest_atype == ATOMIC_TYPE_LONG);
			case ATOMIC_TYPE_ULONG:
				implicit_cast_allowed |=
					(dest_atype == ATOMIC_TYPE_ULONGLONG) ||
					(dest_atype == ATOMIC_TYPE_LONGLONG);
				break;
			case ATOMIC_TYPE_BYTE:
				implicit_cast_allowed |=
					(dest_atype == ATOMIC_TYPE_SHORT);
			case ATOMIC_TYPE_SHORT:
				implicit_cast_allowed |=
					(dest_atype == ATOMIC_TYPE_INT);
			case ATOMIC_TYPE_INT:
				implicit_cast_allowed |=
					(dest_atype == ATOMIC_TYPE_LONG);
			case ATOMIC_TYPE_LONG:
				implicit_cast_allowed |=
					(dest_atype == ATOMIC_TYPE_LONGLONG);
				break;

			case ATOMIC_TYPE_FLOAT:
				implicit_cast_allowed = (dest_atype == ATOMIC_TYPE_DOUBLE);
				break;

			case ATOMIC_TYPE_DOUBLE:
			case ATOMIC_TYPE_LONGLONG:
			case ATOMIC_TYPE_ULONGLONG:
			case ATOMIC_TYPE_INVALID:
			case ATOMIC_TYPE_BOOL:
				implicit_cast_allowed = 0;
				break;
			}
		}
	}

	if(!implicit_cast_allowed) {
		print_error_prefix(source_position);
		fprintf(stderr, "can't implicitely cast ");
		print_type(stderr, from_type);
		fprintf(stderr, " to ");
		print_type(stderr, dest_type);
		fprintf(stderr, "\n");
		return NULL;
	}

	unary_expression_t *cast = allocate_ast(sizeof(cast[0]));
	memset(cast, 0, sizeof(cast[0]));
	cast->expression.type     = EXPR_UNARY;
	cast->type                = UNEXPR_CAST;
	cast->expression.datatype = dest_type;
	cast->value               = from;

	return (expression_t*) cast;
}

static
int is_arithmetic_op(binary_expression_type_t type)
{
	switch(type) {
	case BINEXPR_ADD:
	case BINEXPR_SUB:
	case BINEXPR_MUL:
	case BINEXPR_MOD:
	case BINEXPR_DIV:
	case BINEXPR_SHIFTLEFT:
	case BINEXPR_SHIFTRIGHT:
		return 1;
	default:
		return 0;
	}
}

static
int is_comparison_op(binary_expression_type_t type)
{
	switch(type) {
	case BINEXPR_EQUAL:
	case BINEXPR_NOTEQUAL:
	case BINEXPR_LESS:
	case BINEXPR_LESSEQUAL:
	case BINEXPR_GREATER:
	case BINEXPR_GREATEREQUAL:
		return 1;
	default:
		return 0;
	}
}

static
void check_binary_expression(binary_expression_t *binexpr)
{
	binexpr->left       = check_expression(binexpr->left);
	binexpr->right      = check_expression(binexpr->right);
	expression_t *left  = binexpr->left;
	expression_t *right = binexpr->right;

	type_t *exprtype;
	type_t *lefttype, *righttype;
	binary_expression_type_t binexpr_type = binexpr->type;

	if(binexpr_type == BINEXPR_ASSIGN) {
		check_assign_expression(binexpr);
		exprtype  = left->datatype;
		lefttype  = exprtype;
		righttype = exprtype;
	} else if(is_arithmetic_op(binexpr_type)) {
		exprtype  = left->datatype;
		/* TODO find out greatest common type... */
		lefttype  = exprtype;
		if(binexpr_type == BINEXPR_SHIFTLEFT
				|| binexpr_type == BINEXPR_SHIFTRIGHT) {
			righttype = type_uint;
		} else {
			righttype = exprtype;
		}
	} else if(is_comparison_op(binexpr_type)) {
		exprtype  = type_bool;
		/* TODO find out greatest common type... */
		lefttype  = left->datatype;
		righttype = left->datatype;
	} else {
		abort();
	}

	if(left->datatype != lefttype) {
		binexpr->left  = make_cast(left, lefttype,
		                           binexpr->expression.source_position);
	}
	if(right->datatype != righttype) {
		binexpr->right = make_cast(right, righttype,
		                           binexpr->expression.source_position);
	}
	binexpr->expression.datatype = exprtype;
}

/**
 * find a typeclass instance matching the current type_variable configuration
 */
static
typeclass_instance_t *_find_typeclass_instance(typeclass_t *typeclass,
                                               const source_position_t *pos)
{
	typeclass_instance_t *instance = typeclass->instances;
	while(instance != NULL) {
		assert(instance->typeclass = typeclass);

		type_argument_t *argument  = instance->type_arguments;
		type_variable_t *parameter = typeclass->type_parameters;
		int              match     = 1;
		while(argument != NULL && parameter != NULL) {
			if(parameter->current_type == NULL) {
				print_error_prefix(*pos);
				panic("type variable has no type set while searching "
				      "typeclass instance");
			}
			if(parameter->current_type != argument->type) {
				match = 0;
				break;
			}
			
			argument  = argument->next;
			parameter = parameter->next;
		}
		if(match == 1 && (argument != NULL || parameter != NULL)) {
			print_error_prefix(instance->namespace_entry.source_position);
			panic("type argument count of typeclass instance doesn't match "
			      "type parameter count of typeclass");
		}
		if(match == 1)
			return instance;

		instance = instance->next;
	}

	return NULL;
}

typeclass_instance_t *find_typeclass_instance(typeclass_t *typeclass)
{
	return _find_typeclass_instance(typeclass, NULL);
}

/** tests whether a type variable has a typeclass as constraint */
static
int type_variable_has_constraint(const type_variable_t *type_variable,
                                 const typeclass_t *typeclass)
{
	type_constraint_t *constraint = type_variable->constraints;
	while(constraint != NULL) {
		if(constraint->typeclass == typeclass)
			return 1;

		constraint = constraint->next;
	}

	return 0;
}

typeclass_method_instance_t *get_method_from_typeclass_instance(
		typeclass_instance_t *instance, typeclass_method_t *method)
{
	typeclass_method_instance_t *method_instance = instance->method_instances;
	while(method_instance != NULL) {
		if(method_instance->typeclass_method == method) {
			return method_instance;
		}

		method_instance = method_instance->next;
	}

	return NULL;
}

static
void resolve_typeclass_method_instance(reference_expression_t *ref)
{
	assert(ref->expression.type == EXPR_REFERENCE_TYPECLASS_METHOD);

	typeclass_method_t *typeclass_method = ref->r.typeclass_method;
	typeclass_t        *typeclass        = typeclass_method->typeclass;

	/* test whether 1 of the type variables points to another type variable.
	 * this can happen when typeclass methods are invoked inside polymorphic
	 * methods. We can't resolve the method right now, but we have to check
	 * the constraints of the type variable */
	int cant_resolve = 0;
	type_variable_t *type_var = typeclass->type_parameters;
	while(type_var != NULL) {
		type_t *current_type = type_var->current_type;
		if(current_type == NULL)
			return;

		if(current_type->type == TYPE_REFERENCE_TYPE_VARIABLE) {
			type_reference_t *type_ref      = (type_reference_t*) current_type;
			type_variable_t  *type_variable = type_ref->r.type_variable;

			if(!type_variable_has_constraint(type_variable, typeclass)) {
				print_error_prefix(ref->expression.source_position);
				fprintf(stderr, "type variable '%s' needs a constraint for "
				        "typeclass '%s' when using method '%s'.\n",
				        type_variable->symbol->string,
				        typeclass->symbol->string,
				        typeclass_method->symbol->string);
				return;
			}
			cant_resolve = 1;
		}

		type_var = type_var->next;
	}
	/* we have to defer the resolving for the ast2firm phase */
	if(cant_resolve) {
		return;
	}

	/* we assume that all typevars have current_type set */
	const source_position_t *pos = &ref->expression.source_position;
	typeclass_instance_t *instance = _find_typeclass_instance(typeclass, pos);
	if(instance == NULL) {
		print_error_prefix(ref->expression.source_position);
		fprintf(stderr, "there's no instance of typeclass '%s' for ",
		        typeclass->symbol->string);
		type_variable_t *typevar = typeclass->type_parameters;
		while(typevar != NULL) {
			if(typevar->current_type != NULL) {
				print_type(stderr, typevar->current_type);
				fprintf(stderr, " ");
			}
			typevar = typevar->next;
		}
		fprintf(stderr, "\n");
		return;
	}

	typeclass_method_instance_t *method_instance 
		= get_method_from_typeclass_instance(instance, typeclass_method);
	if(method_instance == NULL) {
		print_error_prefix(ref->expression.source_position);
		fprintf(stderr, "no instance of method '%s' found in typeclass "
		        "instance?\n", typeclass_method->symbol->string);
		panic("panic");
	}

	type_t *type         = (type_t*) method_instance->method->type;
	type_t *pointer_type = make_pointer_type(type);

	ref->expression.type             = EXPR_REFERENCE_TYPECLASS_METHOD_INSTANCE;
	ref->expression.datatype         = pointer_type;
	ref->r.typeclass_method_instance = method_instance;
}

static
void check_type_constraints(type_variable_t *type_variables,
                            const method_t *method_context,
                            const source_position_t source_position)
{
	type_variable_t *type_var     = type_variables;
	while(type_var != NULL) {
		type_constraint_t *constraint   = type_var->constraints;
		type_t            *current_type = type_var->current_type;

		for( ;constraint != NULL; constraint = constraint->next) {
			typeclass_t *typeclass = constraint->typeclass;

			if(typeclass == NULL)
				continue;

			if(current_type->type == TYPE_REFERENCE_TYPE_VARIABLE) {
				type_reference_t *ref      = (type_reference_t*) current_type;
				type_variable_t  *type_var = ref->r.type_variable;

				if(!type_variable_has_constraint(type_var, typeclass)) {
					print_error_prefix(source_position);
					fprintf(stderr, "type variable '%s' needs constraint "
					        "'%s'\n", type_var->symbol->string,
					        typeclass->symbol->string);
				}
				continue;
			}

			/* set typevariable values for the typeclass
			 * This currently only works for typeclasses with 1 parameter */
			typeclass->type_parameters->current_type = type_var->current_type;
		
			typeclass_instance_t *instance 
				= _find_typeclass_instance(typeclass, & source_position);
			if(instance == NULL) {
				print_error_prefix(source_position);
				fprintf(stderr, "concrete type for type variable '%s' of "
				        "method '%s' doesn't match type constraints:\n",
				        type_var->symbol->string,
				        method_context->symbol->string);
				print_error_prefix(source_position);
				fprintf(stderr, "type ");
				print_type(stderr, type_var->current_type);
				fprintf(stderr, " is no instance of typeclass '%s'\n",
				        typeclass->symbol->string);
			}

			/* reset typevar binding */
			typeclass->type_parameters->current_type = NULL;
		}

		type_var = type_var->next;
	}
}

static
void check_call_expression(call_expression_t *call)
{
	call->method                    = check_expression(call->method);
	expression_t    *method         = call->method;
	type_t          *type           = method->datatype;
	type_argument_t *type_arguments = NULL;

	/* can happen if we had a deeper semantic error */
	if(type == NULL)
		return;
	if(type->type != TYPE_POINTER) {
		print_error_prefix(call->expression.source_position);
		fprintf(stderr, "trying to call non-pointer type ");
		print_type(stderr, type);
		fprintf(stderr, "\n");
		return;
	}
	pointer_type_t *pointer_type = (pointer_type_t*) type;

	type = pointer_type->points_to;
	if(type->type != TYPE_METHOD) {
		print_error_prefix(call->expression.source_position);
		fprintf(stderr, "trying to call a non-method value of type");
		print_type(stderr, type);
		fprintf(stderr, "\n");
		return;
	}
	method_type_t *method_type = (method_type_t*) type;

	/* we have to match the parameter types against the type variables in case
	 * of typeclass methods or polymorphic methods
	 */
	type_variable_t *type_variables = NULL;
	if(method->type == EXPR_REFERENCE_TYPECLASS_METHOD) {
		reference_expression_t *ref 
			= (reference_expression_t*) method;
		typeclass_method_t     *typeclass_method = ref->r.typeclass_method;
		typeclass_t            *typeclass        = typeclass_method->typeclass;
		
		type_variables = typeclass->type_parameters;
		type_arguments = ref->type_arguments;
	} else if(method->type == EXPR_REFERENCE_METHOD) {
		reference_expression_t *ref	= (reference_expression_t*) method;
		method_t *referenced_method = ref->r.method;
		type_variables              = referenced_method->type_parameters;
		type_arguments              = ref->type_arguments;
	}

	/* clear typevariable configuration */
	if(type_variables != NULL) {
		type_variable_t *type_var = type_variables;
		while(type_var != NULL) {
			type_var->current_type = NULL;

			type_var = type_var->next;
		}
	}

	/* apply type arguments */
	if(type_arguments != NULL) {
		type_variable_t *type_var      = type_variables;
		type_argument_t *type_argument = type_arguments;
		while(type_argument != NULL && type_var != NULL) {
			type_var->current_type = type_argument->type;

			type_var      = type_var->next;
			type_argument = type_argument->next;
		}

		if(type_argument != NULL || type_var != NULL) {
			error_at(method->source_position,
			         "wrong number of type arguments on method reference");
		}
	}

	/* check call arguments, match argument types against expected types
	 * and try to determine type variable configuration */
	call_argument_t         *argument   = call->arguments;
	method_parameter_type_t *param_type = method_type->parameter_types;
	int                      i          = 0;
	while(argument != NULL) {
		if(param_type == NULL) {
			error_at(call->expression.source_position,
			         "too much arguments for method call\n");
			break;
		}

		argument->expression     = check_expression(argument->expression);
		expression_t *expression = argument->expression;

		type_t       *wanted_type     = param_type->type;
		type_t       *expression_type = expression->datatype;

		/* match type of argument against type variables */
		if(type_variables != NULL && type_arguments == NULL) {
			match_variant_to_concrete_type(wanted_type, expression_type,
			                               expression->source_position);
		} else if(expression_type != wanted_type) {
			expression_t *new_expression 
				= make_cast(expression, wanted_type,
			                expression->source_position);
			if(new_expression == NULL) {
				print_error_prefix(expression->source_position);
				fprintf(stderr, "invalid type for argument %d of call: ", i);
				print_type(stderr, expression->datatype);
				fprintf(stderr, " should be ");
				print_type(stderr, wanted_type);
				fprintf(stderr, "\n");
			} else {
				expression = new_expression;
			}
		}
		argument->expression = expression;

		argument   = argument->next;
		param_type = param_type->next;
		++i;
	}
	if(param_type != NULL) {
		error_at(call->expression.source_position,
		         "too few arguments for method call\n");
	}

	/* test whether we could determine the concrete types for all type
	 * variables */
	type_variable_t *type_var = type_variables;
	while(type_var != NULL) {
		if(type_var->current_type == NULL) {
			print_error_prefix(call->expression.source_position);
			fprintf(stderr, "Couldn't determine concrete type for type "
					"variable '%s' in call expression\n",
			        type_var->symbol->string);
		}
#ifdef DEBUG_TYPEVAR_BINDING
		fprintf(stderr, "TypeVar '%s'(%p) bound to ", type_var->symbol->string,
				type_var->symbol);
		print_type(stderr, type_var->current_type);
		fprintf(stderr, "\n");
#endif

		type_var = type_var->next;
	}
	
	/* normalize result type, as we know the concrete types for the typevars */
	type_t *result_type = method_type->result_type;
	if(type_variables != NULL) {
		int set_type_arguments = 1;
		reference_expression_t *ref = (reference_expression_t*) method;
		type_variable_t        *type_parameters;
		result_type                 = create_concrete_type(result_type);

		if(method->type == EXPR_REFERENCE_TYPECLASS_METHOD) {
			/* we might be able to resolve the typeclass_method_instance now */
			resolve_typeclass_method_instance(ref);
			if(ref->expression.type == EXPR_REFERENCE_TYPECLASS_METHOD_INSTANCE)
				set_type_arguments = 0;

			typeclass_t *typeclass = ref->r.typeclass_method->typeclass;
			type_parameters        = typeclass->type_parameters;
		} else {
			/* check type constraints */
			assert(method->type == EXPR_REFERENCE_METHOD);
			check_type_constraints(type_variables, ref->r.method,
			                       call->expression.source_position);

			type_parameters = ref->r.method->type_parameters;
		}

		/* set type arguments on the reference expression */
		if(set_type_arguments && ref->type_arguments == NULL) {
			type_variable_t *type_var      = type_parameters;
			type_argument_t *last_argument = NULL;
			while(type_var != NULL) {
				type_argument_t *argument = allocate_ast(sizeof(argument[0]));
				memset(argument, 0, sizeof(argument[0]));

				type_t *current_type = type_var->current_type;
				argument->type       = current_type;

				if(last_argument != NULL) {
					last_argument->next = argument;
				} else {
					ref->type_arguments = argument;
				}
				last_argument = argument;
				type_var      = type_var->next;
			}
		}

		ref->expression.datatype 
			= create_concrete_type(ref->expression.datatype);
	}

	/* clear typevariable configuration */
	if(type_variables != NULL) {
		type_variable_t *type_var = type_variables;
		while(type_var != NULL) {
			type_var->current_type = NULL;

#ifdef DEBUG_TYPEVAR_BINDINGS
			fprintf(stderr, "Unbind %s(%p)\n", type_var->symbol->string,
					type_var->symbol);
#endif

			type_var = type_var->next;
		}
	}

	call->expression.datatype = result_type;
}

static
void check_cast_expression(unary_expression_t *cast)
{
	if(cast->expression.datatype == NULL) {
		panic("Cast expression needs a datatype!");
	}
	cast->expression.datatype = normalize_type(cast->expression.datatype);
	cast->value               = check_expression(cast->value);

	if(cast->value->datatype == type_void) {
		error_at(cast->expression.source_position,
		         "can't cast void type to anything\n");
	}
}

static
void check_dereference_expression(unary_expression_t *dereference)
{
	dereference->value  = check_expression(dereference->value);
	expression_t *value = dereference->value;

	if(value->datatype == NULL) {
		error_at(dereference->expression.source_position,
		         "can't derefence expression with unknown datatype\n");
		return;
	}
	if(value->datatype->type != TYPE_POINTER) {
		error_at(dereference->expression.source_position,
		         "can only dereference expressions with pointer type\n");
		return;
	}
	pointer_type_t *pointer_type      = (pointer_type_t*) value->datatype;
	type_t         *dereferenced_type = pointer_type->points_to;
	dereference->expression.datatype  = dereferenced_type;
}

static
void check_unary_expression(unary_expression_t *unary_expression)
{
	switch(unary_expression->type) {
	case UNEXPR_CAST:
		check_cast_expression(unary_expression);
		break;
	case UNEXPR_DEREFERENCE:
		check_dereference_expression(unary_expression);
		break;
	default:
		abort();
		break;
	}
}

static
void check_select_expression(select_expression_t *select)
{
	select->compound       = check_expression(select->compound);
	expression_t *compound = select->compound;

	type_t *datatype = compound->datatype;
	if(datatype == NULL)
		return;

	if(datatype->type != TYPE_POINTER) {
		print_error_prefix(select->expression.source_position);
		fprintf(stderr, "select needs a pointer to compound type but found "
		        "type ");
		print_type(stderr, datatype);
		fprintf(stderr, "\n");
		return;
	}

	pointer_type_t *pointer_type = (pointer_type_t*) datatype;
	
	type_t *points_to = pointer_type->points_to;
	if(points_to->type != TYPE_COMPOUND_STRUCT
			&& points_to->type != TYPE_COMPOUND_UNION) {
		print_error_prefix(select->expression.source_position);
		fprintf(stderr, "select needs a pointer to compound type but found "
		        "type");
		print_type(stderr, datatype);
		fprintf(stderr, "\n");
		return;
	}

	compound_type_t  *compound_type = (compound_type_t*) points_to;
	compound_entry_t *entry         = compound_type->entries;
	symbol_t         *symbol        = select->symbol;
	while(entry != NULL) {
		if(entry->symbol == symbol) {
			break;
		}
		entry = entry->next;
	}
	if(entry == NULL) {
		print_error_prefix(select->expression.source_position);
		fprintf(stderr, "compound type ");
		print_type(stderr, points_to);
		fprintf(stderr, " does not have a member '%s'\n", symbol->string);
		return;
	}

	/* we return a pointer to sub-compounds instead of the compound itself */
	type_t *result_type = entry->type;
	if(result_type->type == TYPE_COMPOUND_STRUCT ||
			result_type->type == TYPE_COMPOUND_UNION) {
		result_type = make_pointer_type(result_type);
	} else if(result_type->type == TYPE_ARRAY) {
		array_type_t *array_type = (array_type_t*) result_type;

		result_type = make_pointer_type(array_type->element_type);
	}

	select->compound_entry      = entry;
	select->expression.datatype = result_type;
}

static
void check_array_access_expression(array_access_expression_t *access)
{
	access->array_ref       = check_expression(access->array_ref);
	access->index           = check_expression(access->index);
	expression_t *array_ref = access->array_ref;
	expression_t *index     = access->index;

	type_t *type = array_ref->datatype;
	if(type == NULL || 
			(type->type != TYPE_POINTER && type->type != TYPE_ARRAY)) {
		print_error_prefix(access->expression.source_position);
		fprintf(stderr, "expected pointer or array type for array access, "
		        "got ");
		print_type(stderr, type);
		fprintf(stderr, "\n");
		return;
	}

	type_t *result_type;
	if(type->type == TYPE_POINTER) {
		pointer_type_t *pointer_type = (pointer_type_t*) type;
		result_type                  = pointer_type->points_to;
	} else {
		assert(type->type == TYPE_ARRAY);
		array_type_t *array_type = (array_type_t*) type;
		result_type              = array_type->element_type;

		/* TODO We could issue a warning if we have a constant index expression
		 * that exceeds the array size
		 */
	}
	access->expression.datatype = result_type;

	if(index->datatype == NULL || !is_type_int(index->datatype)) {
		print_error_prefix(access->expression.source_position);
		fprintf(stderr, "expected integer type for array index, got ");
		print_type(stderr, index->datatype);
		fprintf(stderr, "\n");
		return;
	}

	if(index->datatype != NULL && index->datatype != type_int) {
		access->index = make_cast(index, type_int,
		                          access->expression.source_position);
	}
}

static
void check_sizeof_expression(sizeof_expression_t *expression)
{
	expression->type = normalize_type(expression->type);
	expression->expression.datatype = type_uint;
}

__attribute__((warn_unused_result))
expression_t *check_expression(expression_t *expression)
{
	if(expression == NULL)
		return NULL;

	/* try to lower the expression */
	if((unsigned) expression->type < (unsigned) ARR_LEN(expression_lowerers)) {
		lower_expression_function lowerer 
			= expression_lowerers[expression->type];

		if(lowerer != NULL) {
			expression = lowerer(expression);
		}
	}

	switch(expression->type) {
	case EXPR_INT_CONST:
		expression->datatype = type_int;
		break;
	case EXPR_STRING_CONST:
		expression->datatype = type_byte_ptr;
		break;
	case EXPR_NULL_POINTER:
		expression->datatype = type_void_ptr;
		break;
	case EXPR_REFERENCE:
		check_reference_expression((reference_expression_t*) expression);
		break;
	case EXPR_SIZEOF:
		check_sizeof_expression((sizeof_expression_t*) expression);
		break;
	case EXPR_BINARY:
		check_binary_expression((binary_expression_t*) expression);
		break;
	case EXPR_UNARY:
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
	case EXPR_REFERENCE_VARIABLE:
	case EXPR_REFERENCE_GLOBAL_VARIABLE:
	case EXPR_REFERENCE_CONSTANT:
	case EXPR_REFERENCE_METHOD:
	case EXPR_REFERENCE_METHOD_PARAMETER:
	case EXPR_REFERENCE_TYPECLASS_METHOD:
	case EXPR_REFERENCE_TYPECLASS_METHOD_INSTANCE:
		break;
	case EXPR_LAST:
	case EXPR_INVALID:
		panic("Invalid expression encountered");
	}

	return expression;
}




static
void check_return_statement(return_statement_t *statement)
{
	method_t     *method             = current_method;
	type_t       *method_result_type = method->type->result_type;
	statement->return_value 
		= check_expression(statement->return_value);
	expression_t *return_value       = statement->return_value;

	last_statement_was_return = 1;

	if(return_value != NULL) {
		if(method_result_type == type_void
				&& return_value->datatype != type_void) {
			error_at(statement->statement.source_position,
			         "return with value in void method\n");
			return;
		}

		/* do we need a cast ?*/
		if(return_value->datatype != method_result_type) {
			return_value
				= make_cast(return_value, method_result_type,
				            statement->statement.source_position);

			statement->return_value = return_value;
		}
	} else {
		if(method_result_type != type_void) {
			error_at(statement->statement.source_position,
			         "missing return value in non-void method\n");
			return;
		}
	}
}

static
void check_if_statement(if_statement_t *statement)
{
	statement->condition    = check_expression(statement->condition);
	expression_t *condition = statement->condition;

	if(condition->datatype != type_bool) {
		error_at(statement->statement.source_position,
		         "if condition needs to be of boolean type\n");
		return;
	}

	statement->true_statement = check_statement(statement->true_statement);
	if(statement->false_statement != NULL) {
		statement->false_statement =
			check_statement(statement->false_statement);
	}
}

static
void check_block_statement(block_statement_t *block)
{
	int old_top = environment_top();

	statement_t *statement = block->first_statement;
	statement_t *last      = NULL;
	while(statement != NULL) {
		statement_t *next = statement->next;

		statement = check_statement(statement);
		assert(statement->next == next || statement->next == NULL);
		statement->next = next;

		if(last != NULL) {
			last->next             = statement;
		} else {
			block->first_statement = statement;
		}

		last      = statement;
		statement = next;
	}

	environment_pop_to(old_top);
}

static
void check_variable_declaration(variable_declaration_statement_t *statement)
{
	method_t *method = current_method;
	assert(method != NULL);

	statement->value_number = method->n_local_vars;
	method->n_local_vars++;

	statement->refs         = 0;
	if(statement->type != NULL) {
		statement->type = normalize_type(statement->type);
		check_local_variable_type(statement, statement->type);
	}

	/* push the variable declaration on the environment stack */
	environment_entry_t *entry = environment_push(statement->symbol);
	entry->type                = ENTRY_LOCAL_VARIABLE;
	entry->e.variable          = statement;
}

static
void check_expression_statement(expression_statement_t *statement)
{
	statement->expression    = check_expression(statement->expression);
	expression_t *expression = statement->expression;

	/* can happen on semantic errors */
	if(expression->datatype == NULL)
		return;

	int may_be_unused = 0;
	if(expression->type == EXPR_BINARY &&
			((binary_expression_t*) expression)->type == BINEXPR_ASSIGN) {
		may_be_unused = 1;
	} else if(expression->type == EXPR_CALL) {
		may_be_unused = 1;
	}

	if(expression->datatype != type_void && !may_be_unused) {
		print_warning_prefix(statement->statement.source_position);
		fprintf(stderr, "result of expression is unused\n");
		fprintf(stderr, "note: cast expression to void to avoid this "
		        "warning\n");
	}
}

static
void check_label_statement(label_statement_t *label)
{
	symbol_t *symbol = label->symbol;
	/* anonymous label */
	if(symbol == NULL)
		return;

	environment_entry_t *entry = symbol->label;
	if(entry != NULL && entry->type == ENTRY_LABEL) {
		error_at(label->statement.source_position, "doubled label\n");
		return;
	}

	if(entry == NULL) {
		ARR_APP1(label_stack, symbol);
	}

	/* gotos prior to the label? */
	while(entry != NULL) {
		assert(entry->type == ENTRY_GOTO);
		goto_statement_t *goto_statement = entry->e.goto_statement;

		goto_statement->label = label;
		entry = entry->up;
	}

	environment_entry_t *new_entry
		= obstack_alloc(&label_obstack, sizeof(new_entry[0]));
	memset(new_entry, 0, sizeof(new_entry[0]));
	new_entry->type    = ENTRY_LABEL;
	new_entry->e.label = label;
	symbol->label      = new_entry;
}

static
void check_goto_statement(goto_statement_t *goto_statement)
{
	/* already resolved? */
	if(goto_statement->label != NULL)
		return;

	symbol_t *symbol = goto_statement->label_symbol;
	if(symbol == NULL) {
		error_at(goto_statement->statement.source_position,
		         "unresolved anonymous goto\n");
		return;
	}

	environment_entry_t *entry = symbol->label;
	if(entry != NULL && entry->type == ENTRY_LABEL) {
		goto_statement->label = entry->e.label;
	} else {
		environment_entry_t *new_entry
			= obstack_alloc(&label_obstack, sizeof(new_entry[0]));
		memset(new_entry, 0, sizeof(new_entry[0]));
		new_entry->type             = ENTRY_GOTO;
		new_entry->e.goto_statement = goto_statement;
		new_entry->up               = entry;
		symbol->label               = new_entry;

		if(entry == NULL) {
			ARR_APP1(label_stack, symbol);
		}
	}
}

__attribute__((warn_unused_result))
statement_t *check_statement(statement_t *statement)
{
	if(statement == NULL)
		return NULL;

	/* try to lower the statement */
	if((int) statement->type < (int) ARR_LEN(statement_lowerers)) {
		lower_statement_function lowerer = statement_lowerers[statement->type];

		if(lowerer != NULL) {
			statement = lowerer(statement);
		}
	}

	if(statement == NULL)
		return NULL;

	last_statement_was_return = 0;
	switch(statement->type) {
	case STATEMENT_INVALID:
		panic("encountered invalid statement");
		break;
	case STATEMENT_BLOCK:
		check_block_statement((block_statement_t*) statement);
		break;
	case STATEMENT_RETURN:
		check_return_statement((return_statement_t*) statement);
		break;
	case STATEMENT_GOTO:
		check_goto_statement((goto_statement_t*) statement);
		break;
	case STATEMENT_LABEL:
		check_label_statement((label_statement_t*) statement);
		break;
	case STATEMENT_IF:
		check_if_statement((if_statement_t*) statement);
		break;
	case STATEMENT_VARIABLE_DECLARATION:
		check_variable_declaration((variable_declaration_statement_t*)
		                           statement);
		break;
	case STATEMENT_EXPRESSION:
		check_expression_statement((expression_statement_t*) statement);
		break;
	default:
		panic("Unknown statement found");
		break;
	}

	return statement;
}

static
void check_method(method_t *method)
{
	if(method->is_extern)
		return;

	int old_top            = environment_top();
	current_method         = method;
	char *label_obst_start = obstack_alloc(&label_obstack, 1);

	/* push type variables */
	type_variable_t *type_parameter = method->type_parameters;
	while(type_parameter != NULL) {
		symbol_t            *symbol = type_parameter->symbol;
		environment_entry_t *entry  = environment_push(symbol);
		entry->type                 = ENTRY_TYPE_VARIABLE;
		entry->e.type_variable      = type_parameter;

		assert(type_parameter->current_type == NULL);
		type_parameter = type_parameter->next;
	}

	/* push method parameters */
	method_parameter_t *parameter = method->parameters;
	int n = 0;
	while(parameter != NULL) {
		environment_entry_t *entry = environment_push(parameter->symbol);
		entry->type                = ENTRY_METHOD_PARAMETER;
		entry->e.method_parameter  = parameter;
		parameter->num             = n;
		parameter->type            = normalize_type(parameter->type);

		n++;
		parameter = parameter->next;
	}

	last_statement_was_return = 0;
	if(method->statement != NULL) {
		method->statement = check_statement(method->statement);
	}

	if(!last_statement_was_return) {
		type_t *result_type = method->type->result_type;
		if(result_type != type_void) {
			/* TODO: report end-position of block-statement? */
			print_error_prefix(method->namespace_entry.source_position);
			fprintf(stderr, "missing return statement at end of method '%s'\n",
			        method->symbol->string);
			return;
		}
	}

	/* reset pointers on symbols from labels */
	int len = ARR_LEN(label_stack);
	for(int i = 0; i < len; ++i) {
		symbol_t            *symbol = label_stack[i];
		environment_entry_t *entry  = symbol->label;

		/* this happens when we had a goto to a non-existing label */
		if(entry->type != ENTRY_LABEL) {
			assert(entry->type == ENTRY_GOTO);
			goto_statement_t *goto_statement = entry->e.goto_statement;
			print_error_prefix(goto_statement->statement.source_position);
			fprintf(stderr, "no label '%s' defined in method '%s'\n",
			        symbol->string, current_method->symbol->string);
		}
		symbol->label = NULL;
	}
	ARR_SHRINKLEN(label_stack, 0);
	obstack_free(&label_obstack, label_obst_start);

	current_method = NULL;
	environment_pop_to(old_top);
}

static
void check_constant(constant_t *constant)
{
	expression_t *expression = constant->expression;

	expression = check_expression(expression);
	if(expression->datatype != constant->type) {
		expression = make_cast(expression, constant->type,
		                       constant->namespace_entry.source_position);
	}
	constant->expression = expression;
}

static
void resolve_type_constraint(type_constraint_t *constraint,
                             const source_position_t source_position)
{
	symbol_t            *symbol = constraint->typeclass_symbol;
	environment_entry_t *entry  = symbol->thing;

	if(entry == NULL) {
		print_error_prefix(source_position);
		fprintf(stderr, "nothing known about symbol '%s'\n", symbol->string);
		return;
	}
	if(entry->type != ENTRY_TYPECLASS) {
		print_error_prefix(source_position);
		fprintf(stderr, "expected a typeclass but '%s' is a '%s'\n",
		        symbol->string, get_entry_type_name(entry->type));
		return;
	}

	constraint->typeclass = entry->e.typeclass;
}

static
void resolve_type_variable_constraints(type_variable_t *type_variables,
                                       const source_position_t source_position)
{
	type_variable_t *type_var = type_variables;
	while(type_var != NULL) {
		type_constraint_t *constraint = type_var->constraints;

		while(constraint != NULL) {
			resolve_type_constraint(constraint, source_position);

			constraint = constraint->next;
		}
		type_var = type_var->next;
	}
}

static
void resolve_method_types(method_t *method)
{
	int old_top = environment_top();

	/* push type variables */
	type_variable_t *type_parameter = method->type_parameters;
	while(type_parameter != NULL) {
		symbol_t            *symbol = type_parameter->symbol;
		environment_entry_t *entry  = environment_push(symbol);
		entry->type                 = ENTRY_TYPE_VARIABLE;
		entry->e.type_variable      = type_parameter;

		type_parameter = type_parameter->next;
	}
	resolve_type_variable_constraints(method->type_parameters,
	                                  method->namespace_entry.source_position);

	/* normalize parameter types */
	method_parameter_t *parameter = method->parameters;
	while(parameter != NULL) {
		parameter->type = normalize_type(parameter->type);
		parameter       = parameter->next;
	}

	method->type 
		= (method_type_t*) normalize_type((type_t*) method->type);

	environment_pop_to(old_top);
}

static
void check_typeclass_instance(typeclass_instance_t *instance)
{
	typeclass_method_instance_t *method_instance = instance->method_instances;
	while(method_instance != NULL) {
		method_t *method = method_instance->method;
		check_method(method);

		method_instance = method_instance->next;
	}
}

static
void resolve_typeclass_types(typeclass_t *typeclass)
{
	int old_top            = environment_top();

	/* push type variables */
	type_variable_t *type_parameter = typeclass->type_parameters;
	while(type_parameter != NULL) {
		symbol_t            *symbol = type_parameter->symbol;
		environment_entry_t *entry  = environment_push(symbol);
		entry->type                 = ENTRY_TYPE_VARIABLE;
		entry->e.type_variable      = type_parameter;

		type_parameter = type_parameter->next;
	}
	resolve_type_variable_constraints(typeclass->type_parameters,
	                                typeclass->namespace_entry.source_position);

	/* normalize method types */
	typeclass_method_t *typeclass_method = typeclass->methods;
	while(typeclass_method != NULL) {
		type_t *normalized_type 
			= normalize_type((type_t*) typeclass_method->method_type);
		assert(normalized_type->type == TYPE_METHOD);
		typeclass_method->method_type = (method_type_t*) normalized_type;

		typeclass_method = typeclass_method->next;
	}

	environment_pop_to(old_top);
}


static
void push_typeclass_methods(typeclass_t *typeclass)
{
	typeclass_method_t *method = typeclass->methods;
	while(method != NULL) {
		environment_entry_t *entry = environment_push(method->symbol);
		entry->type                = ENTRY_TYPECLASS_METHOD;
		entry->e.typeclass_method  = method;

		method = method->next;
	}
}

static
void resolve_typeclass_instance(typeclass_instance_t *instance)
{
	symbol_t            *symbol = instance->typeclass_symbol;
	environment_entry_t *entry  = symbol->thing;

	if(entry == NULL) {
		print_error_prefix(instance->namespace_entry.source_position);
		fprintf(stderr, "symbol '%s' is unknown\n", symbol->string);
		return;
	}
	if(entry->type != ENTRY_TYPECLASS) {
		print_error_prefix(instance->namespace_entry.source_position);
		fprintf(stderr, "expected a typeclass but '%s' is a '%s'\n",
		        symbol->string, get_entry_type_name(entry->type));
		return;
	}

	typeclass_t *typeclass = entry->e.typeclass;
	instance->typeclass    = typeclass;
	instance->next         = typeclass->instances;
	typeclass->instances   = instance;

	/* normalize argument types */
	type_argument_t *type_argument = instance->type_arguments;
	while(type_argument != NULL) {
		type_argument->type = normalize_type(type_argument->type);

		type_argument = type_argument->next;
	}

	/* link methods and normalize their types */
	typeclass_method_t *method = typeclass->methods;
	while(method != NULL) {
		int                          found_instance = 0;
		typeclass_method_instance_t *method_instance 
			= instance->method_instances;

		while(method_instance != NULL) {
			method_t *imethod = method_instance->method;

			if(imethod->symbol != method->symbol) {
				method_instance = method_instance->next;
				continue;
			}
			
			if(found_instance) {
				print_error_prefix(imethod->namespace_entry.source_position);
				fprintf(stderr, "multiple implementations of method '%s' found "
				        "in instance of typeclass '%s'\n",
				        method->symbol->string, typeclass->symbol->string);
			} else {
				found_instance                      = 1;
				method_instance->typeclass_method   = method;
				method_instance->typeclass_instance = instance;
			}

			imethod->type 
				= (method_type_t*) normalize_type((type_t*) imethod->type);
			method_instance = method_instance->next;
		}
		if(found_instance == 0) {
			print_error_prefix(instance->namespace_entry.source_position);
			fprintf(stderr, "instance of typeclass '%s' does not implement "
					"method '%s'\n", typeclass->symbol->string,
			        method->symbol->string);
		}

		method = method->next;
	}
}

static
void check_namespace(namespace_t *namespace)
{
	global_variable_t    *variable;
	method_t             *method;
	environment_entry_t  *env_entry;
	typealias_t          *typealias;
	type_t               *type;
	typeclass_t          *typeclass;
	constant_t           *constant;
	int                   old_top = environment_top();

	/* record namespace entries in environment */
	namespace_entry_t *entry = namespace->entries;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_VARIABLE:
			variable            = (global_variable_t*) entry;
			env_entry           = environment_push(variable->symbol);
			env_entry->type     = ENTRY_GLOBAL_VARIABLE;
			env_entry->e.global_variable = variable;
			break;
		case NAMESPACE_ENTRY_METHOD:
			method              = (method_t*) entry;
			env_entry           = environment_push(method->symbol);
			env_entry->type     = ENTRY_METHOD;
			env_entry->e.method = method;
			break;
		case NAMESPACE_ENTRY_TYPEALIAS:
			typealias              = (typealias_t*) entry;
			env_entry              = environment_push(typealias->symbol);
			env_entry->type        = ENTRY_TYPEALIAS;
			env_entry->e.stored_type = (type_t*) typealias->type;
			break;
		case NAMESPACE_ENTRY_TYPECLASS:
			typeclass              = (typeclass_t*) entry;
			env_entry              = environment_push(typeclass->symbol);
			env_entry->type        = ENTRY_TYPECLASS;
			env_entry->e.typeclass = typeclass;
			push_typeclass_methods(typeclass);
			break;
		case NAMESPACE_ENTRY_CONSTANT:
			constant              = (constant_t*) entry;
			env_entry             = environment_push(constant->symbol);
			env_entry->type       = ENTRY_CONSTANT;
			env_entry->e.constant = constant;
			break;
		case NAMESPACE_ENTRY_TYPECLASS_INSTANCE:
			break;
		case NAMESPACE_ENTRY_LAST:
		case NAMESPACE_ENTRY_INVALID:
			panic("invalid namespace entry found");
			break;
		}
		entry = entry->next;
	}

	/* normalize types, resolve typeclass instance references */
	entry = namespace->entries;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_VARIABLE:
			variable            = (global_variable_t*) entry;
			env_entry           = variable->symbol->thing;
			assert(env_entry->e.global_variable == variable);
			variable->type      = normalize_type(variable->type);
			break;
		case NAMESPACE_ENTRY_METHOD:
			method              = (method_t*) entry;
			env_entry           = method->symbol->thing;
			assert(env_entry->e.method == method);
			resolve_method_types(method);
			break;
		case NAMESPACE_ENTRY_TYPEALIAS:
			typealias  = (typealias_t*) entry;
			env_entry  = typealias->symbol->thing;
			type       = env_entry->e.stored_type;
			if(type->type == TYPE_COMPOUND_UNION
					|| type->type == TYPE_COMPOUND_STRUCT) {
				normalize_compound_entries((compound_type_t*) type);
			}
			break;
		case NAMESPACE_ENTRY_TYPECLASS:
			typeclass = (typeclass_t*) entry;
			resolve_typeclass_types(typeclass);
			break;
		case NAMESPACE_ENTRY_TYPECLASS_INSTANCE:
			resolve_typeclass_instance((typeclass_instance_t*) entry);
			break;
		default:
			break;
		}

		entry = entry->next;
	}

	/* check semantics in methods */
	entry = namespace->entries;
	while(entry != NULL) {
		switch(entry->type) {
		case NAMESPACE_ENTRY_METHOD:
			check_method((method_t*) entry);
			break;
		case NAMESPACE_ENTRY_CONSTANT:
			check_constant((constant_t*) entry);
			break;
		case NAMESPACE_ENTRY_TYPECLASS_INSTANCE:
			check_typeclass_instance((typeclass_instance_t*) entry);
			break;
		default:
			break;
		}

		entry = entry->next;
	}

	environment_pop_to(old_top);
}

void register_statement_lowerer(lower_statement_function function,
                                unsigned int statement_type)
{
	unsigned int len = ARR_LEN(statement_lowerers);
	if(statement_type >= len) {
		ARR_RESIZE(statement_lowerers, statement_type + 1);
		memset(&statement_lowerers[len], 0,
		       (statement_type - len + 1) * sizeof(statement_lowerers[0]));
	}

	if(statement_lowerers[statement_type] != NULL) {
		panic("Trying to register multiple lowerers for a statement type");
	}
	statement_lowerers[statement_type] = function;
}

void register_expression_lowerer(lower_expression_function function,
                                 unsigned int expression_type)
{
	unsigned int len = ARR_LEN(expression_lowerers);
	if(expression_type >= len) {
		ARR_RESIZE(expression_lowerers, expression_type + 1);
		memset(&expression_lowerers[len], 0,
		       (expression_type - len + 1) * sizeof(expression_lowerers[0]));
	}

	if(expression_lowerers[expression_type] != NULL) {
		panic("Trying to register multiple lowerers for a expression type");
	}
	expression_lowerers[expression_type] = function;
}

int check_static_semantic(namespace_t *namespace)
{
	obstack_init(&symbol_environment_obstack);
	obstack_init(&label_obstack);

	symbol_stack       = NEW_ARR_F(environment_entry_t*, 0);
	label_stack        = NEW_ARR_F(symbol_t*, 0);
	found_errors       = 0;

	type_bool     = make_atomic_type(ATOMIC_TYPE_BOOL);
	type_byte     = make_atomic_type(ATOMIC_TYPE_BYTE);
	type_int      = make_atomic_type(ATOMIC_TYPE_INT);
	type_uint     = make_atomic_type(ATOMIC_TYPE_UINT);
	type_void_ptr = make_pointer_type(type_void);
	type_byte_ptr = make_pointer_type(type_byte);

	check_namespace(namespace);

	DEL_ARR_F(symbol_stack);
	DEL_ARR_F(label_stack);

	obstack_free(&symbol_environment_obstack, NULL);
	obstack_free(&label_obstack, NULL);

	return !found_errors;
}

void init_semantic_module(void)
{
	statement_lowerers  = NEW_ARR_F(lower_statement_function, 0);
	expression_lowerers = NEW_ARR_F(lower_expression_function, 0);
}

void exit_semantic_module(void)
{
	DEL_ARR_F(expression_lowerers);
	DEL_ARR_F(statement_lowerers);
}

