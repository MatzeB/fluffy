#ifndef TYPE_T_H
#define TYPE_T_H

#include "type.h"
#include "symbol.h"
#include "lexer_t.h"
#include "ast.h"
#include "adt/obst.h"
#include <libfirm/typerep.h>

struct obstack *type_obst;

typedef enum {
	TYPE_INVALID,
	TYPE_VOID,
	TYPE_ATOMIC,
	TYPE_COMPOUND,
	TYPE_METHOD,
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_REFERENCE,
	TYPE_REFERENCE_TYPE_VARIABLE
} type_type_t;

typedef enum {
	ATOMIC_TYPE_INVALID,
	ATOMIC_TYPE_BOOL,
	ATOMIC_TYPE_BYTE,
	ATOMIC_TYPE_UBYTE,
	ATOMIC_TYPE_INT,
	ATOMIC_TYPE_UINT,
	ATOMIC_TYPE_SHORT,
	ATOMIC_TYPE_USHORT,
	ATOMIC_TYPE_LONG,
	ATOMIC_TYPE_ULONG,
	ATOMIC_TYPE_LONGLONG,
	ATOMIC_TYPE_ULONGLONG,
	ATOMIC_TYPE_FLOAT,
	ATOMIC_TYPE_DOUBLE,
} atomic_type_type_t;

struct type_t {
	type_type_t        type;

	ir_type           *firm_type;
};

struct atomic_type_t {
	type_t              type;
	atomic_type_type_t  atype;
};

struct pointer_type_t {
	type_t  type;
	type_t *points_to;
};

struct array_type_t {
	type_t         type;
	type_t        *element_type;
	unsigned long  size;
};

struct type_reference_t {
	type_t             type;
	symbol_t          *symbol;
	source_position_t  source_position;

	union {
		type_variable_t *type_variable;
	} r;
};

struct method_parameter_type_t {
	type_t                  *type;
	method_parameter_type_t *next;
};

struct type_constraint_t {
	symbol_t          *typeclass_symbol;
	typeclass_t       *typeclass;
	type_constraint_t *next;
};

struct type_variable_t {
	type_constraint_t *constraints;
	symbol_t          *symbol;
	type_variable_t   *next;

	type_t            *current_type;
};

struct method_type_t {
	type_t                   type;
	type_t                  *result_type;
	method_parameter_type_t *parameter_types;
	const char              *abi_style;
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
	type_t             type;
	compound_entry_t  *entries;
	symbol_t          *symbol;
	attribute_t       *attributes;
	source_position_t  source_position;
	int                is_union;
};

#endif

