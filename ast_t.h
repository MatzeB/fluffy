#ifndef _AST_T_H_
#define _AST_T_H_

#include "ast.h"
#include "symbol.h"

typedef enum {
	TYPE_ATOMIC,
	TYPE_STRUCT
} type_type_t;

typedef enum {
	ATOMIC_TYPE_INT,
	ATOMIC_TYPE_FLOAT,
	ATOMIC_TYPE_POINTER
} atomic_type_t;

struct type_t {
	type_type_t *type;
};

struct atomic_type_t {
	type_t        type;
	atomic_type_t atype;
	unsigned bits;
	unsigned has_sign : 1;
};

struct statement_t {
};

struct compilation_unit_t {
	environment_t *environment;
};

struct function_t {
	type_t        *returntype;
	statement_t   *statement;
	environment_t *arguments;
};

struct variable_t {
	type_t *type;
};

struct argument_t {
	type_t *type;
};

enum environment_entry_type_t {
	ENTRY_FUNCTION,
	ENTRY_VAR,
	ENTRY_ARGUMENT
};

struct environment_entry_t {
	environment_entry_t      *next;
	symbol_t                 *symbol;
	environment_entry_type_t  type;
	environment_entry_t      *up;
	environment_entry_t      *down;

	union {
		compilation_unit_t *compunit;
		function_t *function;
		variable_t *variable;
	};
};

struct environment_t {
	environment_entry_t *entries;
	int                  entry_count;
	environment_t       *up;
	environment_t       *down;
	unsigned             on_stack : 1;
};

#endif

