#ifndef AST_T_H
#define AST_T_H

#include "ast.h"
#include "ast2firm.h"
#include "symbol.h"
#include "semantic.h"
#include "lexer_t.h"
#include <libfirm/type.h>
#include <libfirm/entity.h>

typedef enum {
	TYPE_INVALID,
	TYPE_VOID,
	TYPE_ATOMIC,
	TYPE_STRUCT,
	TYPE_METHOD,
	TYPE_POINTER,
	TYPE_REFERENCE,
	TYPE_REFERENCE_TYPE,
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
	type_t   type;
	type_t  *points_to;
};

struct type_reference_t {
	type_t             type;
	symbol_t          *symbol;
	source_position_t  source_position;

	union {
		type_t          *type;
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

struct struct_entry_t {
	type_t            *type;
	symbol_t          *symbol;
	struct_entry_t    *next;
	source_position_t  source_position;

	ir_entity         *entity;
};

struct struct_type_t {
	type_t             type;
	struct_entry_t    *entries;
	symbol_t          *symbol;
	source_position_t  source_position;
};

typedef enum {
	EXPR_INVALID = 0,
	EXPR_INT_CONST,
	EXPR_STRING_CONST,
	EXPR_REFERENCE,
	EXPR_REFERENCE_VARIABLE,
	EXPR_REFERENCE_METHOD,
	EXPR_REFERENCE_METHOD_PARAMETER,
	EXPR_REFERENCE_EXTERN_METHOD,
	EXPR_REFERENCE_GLOBAL_VARIABLE,
	EXPR_REFERENCE_TYPECLASS_METHOD,
	EXPR_REFERENCE_TYPECLASS_METHOD_INSTANCE,
	EXPR_CALL,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_SELECT,
	EXPR_ARRAY_ACCESS,
	EXPR_SIZEOF
} expresion_type_t;

struct expression_t {
	expresion_type_t   type;
	type_t            *datatype;
	source_position_t  source_position;
};

struct int_const_t {
	expression_t  expression;
	int           value;
};

struct string_const_t {
	expression_t  expression;
	const char   *value;
};

struct type_argument_t {
	type_t          *type;
	type_argument_t *next;
};

struct reference_expression_t {
	expression_t                      expression;
	symbol_t                         *symbol;
	union {
		variable_declaration_statement_t *variable;
		method_t                         *method;
		extern_method_t                  *extern_method;
		variable_t                       *global_variable;
		method_parameter_t               *method_parameter;
		typeclass_method_t               *typeclass_method;
		typeclass_method_instance_t      *typeclass_method_instance;
	} r;
	type_argument_t *type_arguments;
};

struct call_argument_t {
	expression_t    *expression;
	call_argument_t *next;
};

struct call_expression_t {
	expression_t     expression;
	expression_t    *method;
	call_argument_t *arguments;
};

typedef enum {
	UNEXPR_INVALID = 0,
	UNEXPR_NEGATE,
	UNEXPR_NOT,
	UNEXPR_DEREFERENCE,
	UNEXPR_TAKE_ADDRESS,
	UNEXPR_INCREMENT,
	UNEXPR_DECREMENT,
	UNEXPR_CAST
} unary_expression_type_t;

struct unary_expression_t {
	expression_t             expression;
	unary_expression_type_t  type;
	expression_t            *value;
};

typedef enum {
	BINEXPR_INVALID = 0,
	BINEXPR_ADD,
	BINEXPR_SUB,
	BINEXPR_MUL,
	BINEXPR_DIV,
	BINEXPR_MOD,
	BINEXPR_EQUAL,
	BINEXPR_NOTEQUAL,
	BINEXPR_LESS,
	BINEXPR_LESSEQUAL,
	BINEXPR_GREATER,
	BINEXPR_GREATEREQUAL,
	BINEXPR_AND,
	BINEXPR_OR,
	BINEXPR_XOR,
	BINEXPR_SHIFTLEFT,
	BINEXPR_SHIFTRIGHT,
	BINEXPR_ASSIGN
} binary_expression_type_t;

struct binary_expression_t {
	expression_t              expression;
	binary_expression_type_t  type;
	expression_t             *left;
	expression_t             *right;
};

struct select_expression_t {
	expression_t    expression;
	expression_t   *compound;
	symbol_t       *symbol;

	struct_entry_t *struct_entry;
};

struct array_access_expression_t {
	expression_t  expression;
	expression_t *array_ref;
	expression_t *index;
};

struct sizeof_expression_t {
	expression_t  expression;
	type_t       *type;
};

typedef enum {
	STATEMENT_INVALID,
	STATEMENT_BLOCK,
	STATEMENT_RETURN,
	STATEMENT_VARIABLE_DECLARATION,
	STATEMENT_IF,
	STATEMENT_EXPRESSION,
	STATEMENT_GOTO,
	STATEMENT_LABEL
} statement_type_t;

struct statement_t {
	statement_type_t   type;
	statement_t       *next;
	source_position_t  source_position;
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

	int          value_number; /**< filled in by semantic phase */
	int          refs;
};

struct if_statement_t {
	statement_t   statement;
	expression_t *condition;
	statement_t  *true_statement;
	statement_t  *false_statement;
};

struct goto_statement_t {
	statement_t        statement;
	symbol_t          *label_symbol;
	label_statement_t *label;
};

struct label_statement_t {
	statement_t        statement;
	symbol_t          *symbol;

	ir_node           *block;
	label_statement_t *next;
};

struct expression_statement_t {
	statement_t   statement;
	expression_t *expression;
};

enum namespace_entry_type_t {
	NAMESPACE_ENTRY_METHOD,
	NAMESPACE_ENTRY_VARIABLE,
	NAMESPACE_ENTRY_EXTERN_METHOD,
	NAMESPACE_ENTRY_STRUCT,
	NAMESPACE_ENTRY_TYPECLASS,
	NAMESPACE_ENTRY_TYPECLASS_INSTANCE
};

struct namespace_entry_t {
	namespace_entry_type_t  type;
	namespace_entry_t      *next;
	source_position_t       source_position;
};

struct method_parameter_t {
	method_parameter_t *next;
	symbol_t           *symbol;
	type_t             *type;
	int                 num;
};

#if 0
struct method_instance_t {
	type_t             *concrete_type;
	method_t           *method;
	method_type_t      *type;
	method_parameter_t *parameters;
	statement_t        *statement;

	int                 n_local_vars;
	ir_entity          *entity;
};
#endif

struct method_t {
	namespace_entry_t   namespace_entry;
	symbol_t           *symbol;
	method_type_t      *type;
	type_variable_t    *type_parameters;
	method_parameter_t *parameters;

	statement_t        *statement;

	int                 n_local_vars;
	ir_entity          *entity;
};

struct extern_method_t {
	namespace_entry_t  namespace_entry;
	symbol_t          *symbol;
	method_type_t     *type;

	ir_entity         *entity;
};

struct variable_t {
	namespace_entry_t  namespace_entry;
	symbol_t          *symbol;
	type_t            *type;
};

struct struct_t {
	namespace_entry_t  namespace_entry;
	symbol_t          *symbol;
	struct_type_t     *type;
};

struct typeclass_method_instance_t {
	method_t                    *method;
	typeclass_method_instance_t *next;

	typeclass_method_t          *typeclass_method;
	typeclass_instance_t        *typeclass_instance;
};

struct typeclass_instance_t {
	namespace_entry_t            namespace_entry;

	symbol_t                    *typeclass_symbol;
	typeclass_t                 *typeclass;
	type_argument_t             *type_arguments;
	typeclass_method_instance_t *method_instances;
	typeclass_instance_t        *next;
};

struct typeclass_method_t {
	symbol_t           *symbol;
	method_type_t      *method_type;
	method_parameter_t *parameters;
	typeclass_t        *typeclass;

	typeclass_method_t *next;
};

struct typeclass_t {
	namespace_entry_t     namespace_entry;
	symbol_t             *symbol;

	type_variable_t      *type_parameters;
	typeclass_method_t   *methods;
	typeclass_instance_t *instances;
};

struct namespace_t {
	namespace_entry_t *first_entry;
};

#endif
