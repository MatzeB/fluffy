#ifndef _AST_T_H_
#define _AST_T_H_

#include "ast.h"
#include "symbol.h"

typedef enum {
	TYPE_INVALID,
	TYPE_ATOMIC,
	TYPE_STRUCT
} type_type_t;

typedef enum {
	ATOMIC_TYPE_INVALID,
	ATOMIC_TYPE_INT,
	ATOMIC_TYPE_FLOAT,
	ATOMIC_TYPE_VOID,
	ATOMIC_TYPE_POINTER
} atomic_type_type_t;

struct type_t {
	type_type_t type;
};

struct atomic_type_t {
	type_t              type;
	atomic_type_type_t atype;
	unsigned            bits;
	unsigned    has_sign : 1;
};

typedef enum {
	EXPR_INVALID,
	EXPR_INT_ATOM
} expresion_type_t;

struct expression_t {
	expresion_type_t type;
};

struct int_atom_t {
	expression_t expression;
	int value;
};

typedef enum {
	STATEMENT_INVALID,
	STATEMENT_BLOCK,
	STATEMENT_RETURN
} statement_type_t;

struct statement_t {
	statement_t      *next;
	statement_type_t  type;
};

struct return_statement_t {
	statement_t   statement;
	expression_t *return_value;
};

struct block_statement_t {
	statement_t  statement;
	statement_t *first_statement;
};

enum environment_entry_type_t {
	ENTRY_FUNCTION,
	ENTRY_VARIABLE,
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

struct compilation_unit_t {
	environment_t environment;
};

struct function_t {
	type_t        *return_type;
	statement_t   *statement;
	environment_t *arguments;
};

struct variable_t {
	type_t *type;
};

struct argument_t {
	type_t *type;
};

#endif

