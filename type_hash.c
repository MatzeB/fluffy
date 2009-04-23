#include <config.h>

#include <stdbool.h>
#include "type_hash.h"

#include "adt/error.h"
#include "type_t.h"

#include <assert.h>

#define HashSet         type_hash_t
#define HashSetIterator type_hash_iterator_t
#define ValueType       type_t*
#include "adt/hashset.h"
#undef ValueType
#undef HashSetIterator
#undef HashSet

typedef struct type_hash_iterator_t  type_hash_iterator_t;
typedef struct type_hash_t           type_hash_t;

static unsigned hash_ptr(const void *ptr)
{
	unsigned ptr_int = ((const char*) ptr - (const char*) NULL);
	return ptr_int >> 3;
}

static unsigned hash_atomic_type(const atomic_type_t *type)
{
	unsigned some_prime = 27644437;

	return type->atype * some_prime;
}

static unsigned hash_pointer_type(const pointer_type_t *type)
{
	return hash_ptr(type->points_to);
}

static unsigned hash_array_type(const array_type_t *type)
{
	unsigned some_prime = 27644437;
	return hash_ptr(type->element_type) ^ (type->size * some_prime);
}

static unsigned hash_compound_type(const compound_type_t *type)
{
	unsigned result = hash_ptr(type->symbol);

	return result;
}

static unsigned hash_type(const type_t *type);

static unsigned hash_method_type(const method_type_t *type)
{
	unsigned result = hash_ptr(type->result_type);

	method_parameter_type_t *parameter = type->parameter_types;
	while(parameter != NULL) {
		result ^= hash_ptr(parameter->type);
		parameter = parameter->next;
	}
	if(type->variable_arguments)
		result = ~result;

	return result;
}

static unsigned hash_type_reference_type_variable(const type_reference_t *type)
{
	return hash_ptr(type->type_variable);
}

static unsigned hash_bind_typevariables_type_t(const bind_typevariables_type_t *type)
{
	unsigned hash = hash_compound_type(type->polymorphic_type);
	type_argument_t *argument = type->type_arguments;
	while(argument != NULL) {
		hash ^= hash_type(argument->type);
		argument = argument->next;
	}

	return hash;
}

static unsigned hash_type(const type_t *type)
{
	switch(type->type) {
	case TYPE_INVALID:
	case TYPE_VOID:
	case TYPE_ERROR:
	case TYPE_REFERENCE:
		panic("internalizing void or invalid types not possible");
	case TYPE_REFERENCE_TYPE_VARIABLE:
		return hash_type_reference_type_variable(
				(const type_reference_t*) type);
	case TYPE_ATOMIC:
		return hash_atomic_type((const atomic_type_t*) type);
	case TYPE_TYPEOF: {
		const typeof_type_t *typeof_type = (const typeof_type_t*) type;
		return hash_ptr(typeof_type->expression);
	}
	case TYPE_COMPOUND_CLASS:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return hash_compound_type((const compound_type_t*) type);
	case TYPE_METHOD:
		return hash_method_type((const method_type_t*) type);
	case TYPE_POINTER:
		return hash_pointer_type((const pointer_type_t*) type);
	case TYPE_ARRAY:
		return hash_array_type((const array_type_t*) type);
	case TYPE_BIND_TYPEVARIABLES:
		return hash_bind_typevariables_type_t(
				(const bind_typevariables_type_t*) type);
	}
	abort();
}

static bool atomic_types_equal(const atomic_type_t *type1,
                               const atomic_type_t *type2)
{
	return type1->atype == type2->atype;
}

static bool compound_types_equal(const compound_type_t *type1,
                                 const compound_type_t *type2)
{
	if(type1->symbol != type2->symbol)
		return false;
	/* TODO: check type parameters? */

	return true;
}

static bool method_types_equal(const method_type_t *type1,
                               const method_type_t *type2)
{
	if(type1->result_type != type2->result_type)
		return false;

	if(type1->variable_arguments != type2->variable_arguments)
		return false;

	method_parameter_type_t *param1 = type1->parameter_types;
	method_parameter_type_t *param2 = type2->parameter_types;
	while(param1 != NULL && param2 != NULL) {
		if(param1->type != param2->type)
			return false;
		param1 = param1->next;
		param2 = param2->next;
	}
	if(param1 != NULL || param2 != NULL)
		return false;

	return true;
}

static bool pointer_types_equal(const pointer_type_t *type1,
                                const pointer_type_t *type2)
{
	return type1->points_to == type2->points_to;
}

static bool array_types_equal(const array_type_t *type1,
                              const array_type_t *type2)
{
	return type1->element_type == type2->element_type
		&& type1->size == type2->size;
}

static bool type_references_type_variable_equal(const type_reference_t *type1,
                                                const type_reference_t *type2)
{
	return type1->type_variable == type2->type_variable;
}

static bool bind_typevariables_type_equal(const bind_typevariables_type_t*type1,
                                          const bind_typevariables_type_t*type2)
{
	if(type1->polymorphic_type != type2->polymorphic_type)
		return false;

	type_argument_t *argument1 = type1->type_arguments;
	type_argument_t *argument2 = type2->type_arguments;
	while(argument1 != NULL) {
		if(argument2 == NULL)
			return false;
		if(argument1->type != argument2->type)
			return false;
		argument1 = argument1->next;
		argument2 = argument2->next;
	}
	if(argument2 != NULL)
		return false;

	return true;
}

static bool types_equal(const type_t *type1, const type_t *type2)
{
	if(type1 == type2)
		return true;
	if(type1->type != type2->type)
		return false;

	switch(type1->type) {
	case TYPE_INVALID:
	case TYPE_VOID:
	case TYPE_ERROR:
	case TYPE_REFERENCE:
		return false;
	case TYPE_TYPEOF: {
		const typeof_type_t *typeof_type1 = (const typeof_type_t*) type1;
		const typeof_type_t *typeof_type2 = (const typeof_type_t*) type2;
		return typeof_type1->expression == typeof_type2->expression;
	}
	case TYPE_REFERENCE_TYPE_VARIABLE:
		return type_references_type_variable_equal(
				(const type_reference_t*) type1,
		        (const type_reference_t*) type2);
	case TYPE_ATOMIC:
		return atomic_types_equal((const atomic_type_t*) type1,
		                          (const atomic_type_t*) type2);
	case TYPE_COMPOUND_CLASS:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return compound_types_equal((const compound_type_t*) type1,
		                            (const compound_type_t*) type2);
	case TYPE_METHOD:
		return method_types_equal((const method_type_t*) type1,
		                          (const method_type_t*) type2);
	case TYPE_POINTER:
		return pointer_types_equal((const pointer_type_t*) type1,
		                           (const pointer_type_t*) type2);
	case TYPE_ARRAY:
		return array_types_equal((const array_type_t*) type1,
		                         (const array_type_t*) type2);
	case TYPE_BIND_TYPEVARIABLES:
		return bind_typevariables_type_equal(
				(const bind_typevariables_type_t*) type1,
				(const bind_typevariables_type_t*) type2);
	}
	panic("invalid type encountered");
}

#define HashSet                    type_hash_t
#define HashSetIterator            type_hash_iterator_t
#define ValueType                  type_t*
#define NullValue                  NULL
#define DeletedValue               ((type_t*)-1)
#define Hash(this, key)            hash_type(key)
#define KeysEqual(this,key1,key2)  types_equal(key1, key2)
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(*(ptr)))

#define hashset_init             _typehash_init
#define hashset_init_size        _typehash_init_size
#define hashset_destroy          _typehash_destroy
#define hashset_insert           _typehash_insert
#define hashset_remove           typehash_remove
#define hashset_find             typehash_find
#define hashset_size             typehash_size
#define hashset_iterator_init    typehash_iterator_init
#define hashset_iterator_next    typehash_iterator_next
#define hashset_remove_iterator  typehash_remove_iterator
#define SCALAR_RETURN

#include "adt/hashset.c"

static type_hash_t typehash;

void init_typehash(void)
{
	_typehash_init(&typehash);
}

void exit_typehash(void)
{
	_typehash_destroy(&typehash);
}

type_t *typehash_insert(type_t *type)
{
	return _typehash_insert(&typehash, type);
}

int typehash_contains(type_t *type)
{
	return typehash_find(&typehash, type) != NULL;
}
