#ifndef TYPE_T_H
#define TYPE_T_H

#include <stdbool.h>
#include "type.h"
#include "symbol.h"
#include "lexer.h"
#include "ast.h"
#include "ast_t.h"
#include "adt/obst.h"
#include <libfirm/typerep.h>

struct obstack *type_obst;

typedef enum {
	TYPE_INVALID,
	TYPE_ERROR,
	TYPE_VOID,
	TYPE_ATOMIC,
	TYPE_COMPOUND_STRUCT,
	TYPE_COMPOUND_UNION,
	TYPE_FUNCTION,
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_TYPEOF,
	TYPE_REFERENCE,
	TYPE_REFERENCE_TYPE_VARIABLE,
	TYPE_BIND_TYPEVARIABLES,
	TYPE_LAST = TYPE_BIND_TYPEVARIABLES
} type_kind_t;

typedef enum {
	ATOMIC_TYPE_INVALID,
	ATOMIC_TYPE_BOOL,
	ATOMIC_TYPE_BYTE,
	ATOMIC_TYPE_UBYTE,
	ATOMIC_TYPE_SHORT,
	ATOMIC_TYPE_USHORT,
	ATOMIC_TYPE_INT,
	ATOMIC_TYPE_UINT,
	ATOMIC_TYPE_LONG,
	ATOMIC_TYPE_ULONG,
	ATOMIC_TYPE_LONGLONG,
	ATOMIC_TYPE_ULONGLONG,
	ATOMIC_TYPE_FLOAT,
	ATOMIC_TYPE_DOUBLE,
} atomic_type_kind_t;

struct type_base_t {
	type_kind_t  kind;
	ir_type     *firm_type;
};

struct atomic_type_t {
	type_base_t         base;
	atomic_type_kind_t  akind;
};

struct pointer_type_t {
	type_base_t  base;
	type_t      *points_to;
};

struct array_type_t {
	type_base_t    base;
	type_t        *element_type;
	expression_t  *size_expression;
};

struct typeof_type_t {
	type_base_t   base;
	expression_t *expression;
};

struct type_argument_t {
	type_t          *type;
	type_argument_t *next;
};

struct type_reference_t {
	type_base_t        base;
	symbol_t          *symbol;
	source_position_t  source_position;
	type_argument_t   *type_arguments;

	type_variable_t   *type_variable;
};

struct bind_typevariables_type_t {
	type_base_t      base;
	type_argument_t *type_arguments;
	compound_type_t *polymorphic_type;
};

struct type_constraint_t {
	symbol_t          *concept_symbol;
	concept_t         *concept;
	type_constraint_t *next;
};

struct function_type_t {
	type_base_t  base;
	type_t      *argument_type;
	type_t      *result_type;
};

struct effect_type_t {
	type_base_t  base;
	type_t      *data_type;
};

struct compound_entry_t {
	type_t            *type;
	symbol_t          *symbol;
	compound_entry_t  *next;
	attribute_t       *attributes;
	source_position_t  source_position;

	ir_entity         *entity;
};

struct compound_type_t {
	type_base_t        base;
	compound_entry_t  *entries;
	symbol_t          *symbol;
	attribute_t       *attributes;
	type_variable_t   *type_parameters;
	context_t          context;
	source_position_t  source_position;
};

union type_t
{
	type_kind_t                kind;
	type_base_t                base;
	atomic_type_t              atomic;
	pointer_type_t             pointer;
	array_type_t               array;
	typeof_type_t              typeof;
	type_reference_t           reference;
	bind_typevariables_type_t  bind_typevariables;
	function_type_t            function;
	compound_type_t            compound;
};

type_t *allocate_type(type_kind_t kind);
type_t *make_atomic_type(atomic_type_kind_t type);
type_t *make_pointer_type(type_t *type);

static inline bool is_type_array(const type_t *type)
{
	return type->kind == TYPE_ARRAY;
}

#endif

