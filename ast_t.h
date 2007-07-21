#ifndef AST_T_H
#define AST_T_H

#include "ast.h"
#include "ast2firm.h"
#include "symbol.h"
#include "semantic.h"
#include "lexer.h"
#include "type.h"
#include "adt/obst.h"
#include <libfirm/typerep.h>

extern struct obstack  ast_obstack;
extern namespace_t    *namespaces;

typedef enum {
	DECLARATION_INVALID,
	DECLARATION_METHOD,
	DECLARATION_METHOD_PARAMETER,
	DECLARATION_VARIABLE,
	DECLARATION_CONSTANT,
	DECLARATION_TYPE_VARIABLE,
	DECLARATION_TYPEALIAS,
	DECLARATION_TYPECLASS,
	DECLARATION_TYPECLASS_METHOD,
	DECLARATION_LABEL,
	DECLARATION_LAST
} declaration_type_t;

/**
 * base struct for a declaration
 */
struct declaration_t {
	declaration_type_t  type;
	symbol_t           *symbol;
	declaration_t      *next;
	source_position_t   source_position;
};

struct export_t {
	symbol_t          *symbol;
	export_t          *next;
	source_position_t  source_position;
};

/**
 * a naming context. Containts a list of declarations valid in this context
 * (note that contexts are hierarchic, declarations from parent contexts are
 *  not explicitely included)
 */
struct context_t {
	declaration_t        *declarations;
	typeclass_instance_t *typeclass_instances;
	export_t             *exports;
};

/**
 * base structure for attributes (meta-data which can be attached to several
 * language elements)
 */
struct attribute_t {
	unsigned           type;
	source_position_t  source_position;
	attribute_t       *next;
};

struct type_variable_t {
	declaration_t      declaration;
	type_constraint_t *constraints;
	type_variable_t   *next;

	type_t            *current_type;
};

struct method_t {
	method_type_t      *type;
	type_variable_t    *type_parameters;
	method_parameter_t *parameters;
	unsigned char       export;
	unsigned char       is_extern;

	context_t           context;
	statement_t        *statement;

	union {
		ir_entity      *entity;
		ir_entity     **entities;
	} e;
	int                 n_local_vars;
};

struct method_declaration_t {
	declaration_t  declaration;
	method_t       method;
};

typedef enum {
	EXPR_INVALID = 0,
	EXPR_INT_CONST,
	EXPR_BOOL_CONST,
	EXPR_STRING_CONST,
	EXPR_NULL_POINTER,
	EXPR_REFERENCE,
	EXPR_CALL,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_SELECT,
	EXPR_ARRAY_ACCESS,
	EXPR_SIZEOF,
	EXPR_FUNC,
	EXPR_LAST
} expresion_type_t;

/**
 * base structure for expressions
 */
struct expression_t {
	expresion_type_t   type;
	type_t            *datatype;
	source_position_t  source_position;
};

struct bool_const_t {
	expression_t  expression;
	int           value;
};

struct int_const_t {
	expression_t  expression;
	int           value;
};

struct string_const_t {
	expression_t  expression;
	const char   *value;
};

struct null_pointer_t {
	expression_t  expression;
};

struct func_expression_t {
	expression_t  expression;
	method_t      method;
};

struct type_argument_t {
	type_t          *type;
	type_argument_t *next;
};

struct reference_expression_t {
	expression_t     expression;
	symbol_t        *symbol;
	declaration_t   *declaration;
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
	UNEXPR_BITWISE_NOT,
	UNEXPR_DEREFERENCE,
	UNEXPR_TAKE_ADDRESS,
	UNEXPR_CAST
} unary_expression_type_t;

struct unary_expression_t {
	expression_t             expression;
	unary_expression_type_t  type;
	expression_t            *value;
};

typedef enum {
	BINEXPR_INVALID = 0,
	BINEXPR_ASSIGN,
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
	BINEXPR_LAZY_AND,
	BINEXPR_LAZY_OR,
	BINEXPR_AND,
	BINEXPR_OR,
	BINEXPR_XOR,
	BINEXPR_SHIFTLEFT,
	BINEXPR_SHIFTRIGHT,
} binary_expression_type_t;

struct binary_expression_t {
	expression_t              expression;
	binary_expression_type_t  type;
	expression_t             *left;
	expression_t             *right;
};

struct select_expression_t {
	expression_t      expression;
	expression_t     *compound;
	symbol_t         *symbol;

	compound_entry_t *compound_entry;
	declaration_t    *declaration;
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
	STATEMENT_LABEL,
	STATEMENT_LAST
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
	statement_t        statement;
	statement_t       *statements;
	source_position_t  end_position;
	context_t          context;
};

struct variable_declaration_t {
	declaration_t  declaration;
	type_t        *type;

	unsigned char  is_extern;
	unsigned char  export;
	unsigned char  is_global;
	unsigned char  needs_entity;
	int            refs;         /**< temporarily used by semantic phase */

	ir_entity     *entity;
	int            value_number;
};

struct variable_declaration_statement_t {
	statement_t             statement;
	variable_declaration_t  declaration;
};

struct if_statement_t {
	statement_t   statement;
	expression_t *condition;
	statement_t  *true_statement;
	statement_t  *false_statement;
};

struct label_declaration_t {
	declaration_t        declaration;
	ir_node             *block;
	label_declaration_t *next;
};

struct goto_statement_t {
	statement_t          statement;
	symbol_t            *label_symbol;
	label_declaration_t *label;
};

struct label_statement_t {
	statement_t          statement;
	label_declaration_t  declaration;
};

struct expression_statement_t {
	statement_t   statement;
	expression_t *expression;
};




struct method_parameter_t {
	declaration_t       declaration;
	method_parameter_t *next;
	type_t             *type;
	int                 num;
};

struct constant_t {
	declaration_t  declaration;
	type_t        *type;
	expression_t  *expression;
};

struct typealias_t {
	declaration_t  declaration;
	type_t        *type;
};

struct typeclass_method_instance_t {
	method_t                     method;
	symbol_t                    *symbol;
	source_position_t            source_position;
	typeclass_method_instance_t *next;

	typeclass_method_t          *typeclass_method;
	typeclass_instance_t        *typeclass_instance;
};

struct typeclass_instance_t {
	symbol_t                    *typeclass_symbol;
	source_position_t            source_position;
	typeclass_t                 *typeclass;
	type_argument_t             *type_arguments;
	typeclass_method_instance_t *method_instances;
	typeclass_instance_t        *next;
	typeclass_instance_t        *next_in_typeclass;
};

struct typeclass_method_t {
	declaration_t       declaration;
	method_type_t      *method_type;
	method_parameter_t *parameters;
	typeclass_t        *typeclass;

	typeclass_method_t *next;
};

struct typeclass_t {
	declaration_t         declaration;

	type_variable_t      *type_parameters;
	typeclass_method_t   *methods;
	typeclass_instance_t *instances;
	context_t             context;
};

struct namespace_t {
	symbol_t             *symbol;
	const char           *filename;

	context_t             context;

	namespace_t          *next;
};

static inline
void *_allocate_ast(size_t size)
{
	return obstack_alloc(&ast_obstack, size);
}

#define allocate_ast(size)                 _allocate_ast(size)

const char *get_declaration_type_name(declaration_type_t type);

/* ----- helpers for plugins ------ */

unsigned register_expression();
unsigned register_statement();
unsigned register_declaration();
unsigned register_attribute();

#endif
