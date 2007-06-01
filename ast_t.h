#ifndef AST_T_H
#define AST_T_H

#include "ast.h"
#include "ast2firm.h"
#include "symbol.h"
#include "semantic.h"
#include "lexer_t.h"
#include "type.h"
#include <libfirm/typerep.h>

typedef enum {
	EXPR_INVALID = 0,
	EXPR_INT_CONST,
	EXPR_STRING_CONST,
	EXPR_REFERENCE,
	EXPR_REFERENCE_VARIABLE,
	EXPR_REFERENCE_METHOD,
	EXPR_REFERENCE_METHOD_PARAMETER,
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
		global_variable_t                *global_variable;
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
	NAMESPACE_ENTRY_TYPEALIAS,
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

struct method_t {
	namespace_entry_t   namespace_entry;
	symbol_t           *symbol;
	method_type_t      *type;
	type_variable_t    *type_parameters;
	method_parameter_t *parameters;
	int                 is_constructor;
	int                 is_extern;

	statement_t        *statement;

	int                 n_local_vars;
	ir_entity          *entity;
};

struct global_variable_t {
	namespace_entry_t  namespace_entry;
	symbol_t          *symbol;
	type_t            *type;
	int                is_extern;

	ir_entity         *entity;
};

struct typealias_t {
	namespace_entry_t  namespace_entry;
	symbol_t          *symbol;
	type_t            *type;
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
	namespace_entry_t *entries;
};

#endif
