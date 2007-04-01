#ifndef _AST_T_H_
#define _AST_T_H_

#include "ast.h"
#include "symbol.h"
#include "firm/tr/type.h"

typedef enum {
	TYPE_INVALID,
	TYPE_ATOMIC,
	TYPE_STRUCT,
	TYPE_REF,
} type_type_t;

typedef enum {
	ATOMIC_TYPE_INVALID,
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
	ATOMIC_TYPE_VOID,
	ATOMIC_TYPE_POINTER
} atomic_type_type_t;

struct type_t {
	type_type_t  type;
	ir_type     *firm_type;
};

struct atomic_type_t {
	type_t              type;
	atomic_type_type_t atype;
};

struct ref_type_t {
	type_t     type;
	symbol_t  *symbol;
};

typedef enum {
	EXPR_INVALID,
	EXPR_INT_CONST,
	EXPR_CAST,
	EXPR_VARIABLE_REFERENCE
} expresion_type_t;

struct expression_t {
	expresion_type_t  type;
	type_t           *datatype;
};

struct int_const_t {
	expression_t  expression;
	int           value;
};

struct cast_expression_t {
	expression_t  expression;
	expression_t *value;
};

struct variable_reference_expression_t {
	expression_t    expression;
	symbol_t       *symbol;
	variable_declaration_statement_t *declaration;
};

struct call_expression_t {
	expression_t  expression;
	/* TODO arguments */
};

typedef enum {
	STATEMENT_INVALID,
	STATEMENT_BLOCK,
	STATEMENT_RETURN,
	STATEMENT_VARIABLE_DECLARATION,
	STATEMENT_IF,
	STATEMENT_EXPRESSION
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

struct variable_declaration_statement_t {
	statement_t  statement;
	type_t      *type;
	symbol_t    *symbol;
};

struct if_statement_t {
	statement_t   statement;
	expression_t *condition;
	statement_t  *true_statement;
	statement_t  *false_statement;
};

struct expression_statement_t {
	statement_t   statement;
	expression_t *expression;
};

enum namespace_entry_type_t {
	NAMESPACE_ENTRY_FUNCTION,
	NAMESPACE_ENTRY_VARIABLE
};

struct namespace_entry_t {
	namespace_entry_type_t  type;
	namespace_entry_t      *next;
};

struct function_t {
	namespace_entry_t  namespace_entry;
	symbol_t          *symbol;
	type_t            *return_type;
	statement_t       *statement;
	/* TODO arguments */
};

struct variable_t {
	namespace_entry_t  namespace_entry;
	symbol_t          *symbol;
	type_t            *type;
};

struct namespace_t {
	namespace_entry_t *first_entry;
};

#endif

