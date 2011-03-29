#ifndef AST_T_H
#define AST_T_H

#include <stdbool.h>
#include "ast.h"
#include "ast2firm.h"
#include "symbol.h"
#include "semantic.h"
#include "lexer.h"
#include "type.h"
#include "adt/obst.h"
#include <libfirm/typerep.h>

extern struct obstack  ast_obstack;

extern module_t *modules;

/**
 * Operator precedence classes
 */
typedef enum precedence_t {
	PREC_BOTTOM,
	PREC_ASSIGNMENT,
	PREC_LAZY_OR,
	PREC_LAZY_AND,
	PREC_OR,
	PREC_XOR,
	PREC_AND,
	PREC_EQUALITY,
	PREC_RELATIONAL,
	PREC_ADDITIVE,
	PREC_MULTIPLICATIVE,
	PREC_CAST,
	PREC_UNARY,
	PREC_POSTFIX,
	PREC_TOP
} precedence_t;

typedef enum declaration_kind_t {
	ENTITY_INVALID,
	ENTITY_ERROR,
	ENTITY_FUNCTION,
	ENTITY_FUNCTION_PARAMETER,
	ENTITY_TYPE_VARIABLE,
	ENTITY_CONCEPT_FUNCTION,
	ENTITY_CONCEPT,
	ENTITY_VARIABLE,
	ENTITY_CONSTANT,
	ENTITY_TYPEALIAS,
	ENTITY_LABEL,
	ENTITY_LAST = ENTITY_LABEL,
} entity_kind_t;

/**
 * a named entity is something which can be referenced by its name
 * (a symbol)
 */
typedef struct entity_base_t {
	entity_kind_t      kind;
	symbol_t          *symbol;
	source_position_t  source_position;
	entity_t          *next;
	bool               exported : 1;
	int                refs;         /**< temporarily used by semantic phase */
} entity_base_t;

struct export_t {
	symbol_t          *symbol;
	export_t          *next;
	source_position_t  source_position;
};

struct import_t {
	symbol_t          *module;
	symbol_t          *symbol;
	import_t          *next;
	source_position_t  source_position;
};

/**
 * a naming context. Containts a list of declarations valid in this context
 * (note that contexts are hierarchic, declarations from parent contexts are
 *  not explicitely included)
 */
struct context_t {
	entity_t           *entities;
	concept_instance_t *concept_instances;
	export_t           *exports;
	import_t           *imports;
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
	entity_base_t      base;
	type_constraint_t *constraints;
	type_variable_t   *next;

	type_t            *current_type;
};

struct function_parameter_t {
	entity_base_t         base;
	function_parameter_t *next;
	type_t               *type;
	int                   num;
};

struct function_t {
	function_type_t     *type;
	type_variable_t     *type_parameters;
	function_parameter_t *parameters;
	bool                  is_extern;
	
	context_t    context;
	statement_t *statement;

	union {
		ir_entity   *entity;
		ir_entity  **entities;
	} e;
	unsigned n_local_vars;
};

struct function_entity_t {
	entity_base_t  base;
	function_t     function;
};

struct variable_t {
	entity_base_t  base;
	type_t        *type;
	bool           is_extern;
	bool           export;
	bool           is_global;
	bool           needs_entity;

	ir_entity     *entity;
	int            value_number;
};

struct label_t {
	entity_base_t base;

	ir_node *block;
	label_t *next;
};

struct constant_t {
	entity_base_t  base;
	type_t        *type;
	expression_t  *expression;
};

struct typealias_t {
	entity_base_t  base;
	type_t        *type;
};

struct concept_function_t {
	entity_base_t         base;
	function_type_t      *type;
	function_parameter_t *parameters;
	concept_t            *concept;

	concept_function_t   *next;
};

struct concept_t {
	entity_base_t       base;

	type_variable_t    *type_parameters;
	concept_function_t *functions;
	concept_instance_t *instances;
	context_t           context;
};

union entity_t {
	entity_kind_t        kind;
	entity_base_t        base;
	type_variable_t      type_variable;
	variable_t           variable;
	function_entity_t    function;
	function_parameter_t parameter;
	constant_t           constant;
	label_t              label;
	typealias_t          typealias;
	concept_t            concept;
	concept_function_t   concept_function;
};

struct module_t {
	symbol_t  *name;
	context_t  context;
	module_t  *next;
	bool       processing : 1;
	bool       processed : 1;
};

typedef enum {
	EXPR_INVALID = 0,
	EXPR_ERROR,
	EXPR_INT_CONST,
	EXPR_FLOAT_CONST,
	EXPR_BOOL_CONST,
	EXPR_STRING_CONST,
	EXPR_NULL_POINTER,
	EXPR_REFERENCE,
	EXPR_CALL,
	EXPR_SELECT,
	EXPR_ARRAY_ACCESS,
	EXPR_SIZEOF,
	EXPR_FUNC,

	EXPR_UNARY_FIRST,
	EXPR_UNARY_NEGATE = EXPR_UNARY_FIRST,
	EXPR_UNARY_NOT,
	EXPR_UNARY_BITWISE_NOT,
	EXPR_UNARY_DEREFERENCE,
	EXPR_UNARY_TAKE_ADDRESS,
	EXPR_UNARY_CAST,
	EXPR_UNARY_INCREMENT,
	EXPR_UNARY_DECREMENT,
	EXPR_UNARY_LAST = EXPR_UNARY_DECREMENT,

	EXPR_BINARY_FIRST,
	EXPR_BINARY_ASSIGN = EXPR_BINARY_FIRST,
	EXPR_BINARY_ADD,
	EXPR_BINARY_SUB,
	EXPR_BINARY_MUL,
	EXPR_BINARY_DIV,
	EXPR_BINARY_MOD,
	EXPR_BINARY_EQUAL,
	EXPR_BINARY_NOTEQUAL,
	EXPR_BINARY_LESS,
	EXPR_BINARY_LESSEQUAL,
	EXPR_BINARY_GREATER,
	EXPR_BINARY_GREATEREQUAL,
	EXPR_BINARY_LAZY_AND,
	EXPR_BINARY_LAZY_OR,
	EXPR_BINARY_AND,
	EXPR_BINARY_OR,
	EXPR_BINARY_XOR,
	EXPR_BINARY_SHIFTLEFT,
	EXPR_BINARY_SHIFTRIGHT,
	EXPR_BINARY_LAST = EXPR_BINARY_SHIFTRIGHT,

	EXPR_LAST = EXPR_BINARY_LAST
} expression_kind_t;

#define EXPR_UNARY_CASES           \
	case EXPR_UNARY_NEGATE:        \
	case EXPR_UNARY_NOT:           \
	case EXPR_UNARY_BITWISE_NOT:   \
	case EXPR_UNARY_DEREFERENCE:   \
	case EXPR_UNARY_TAKE_ADDRESS:  \
	case EXPR_UNARY_CAST:          \
	case EXPR_UNARY_INCREMENT:     \
	case EXPR_UNARY_DECREMENT:

#define EXPR_BINARY_CASES          \
	case EXPR_BINARY_ASSIGN:       \
	case EXPR_BINARY_ADD:          \
	case EXPR_BINARY_SUB:          \
	case EXPR_BINARY_MUL:          \
	case EXPR_BINARY_DIV:          \
	case EXPR_BINARY_MOD:          \
	case EXPR_BINARY_EQUAL:        \
	case EXPR_BINARY_NOTEQUAL:     \
	case EXPR_BINARY_LESS:         \
	case EXPR_BINARY_LESSEQUAL:    \
	case EXPR_BINARY_GREATER:      \
	case EXPR_BINARY_GREATEREQUAL: \
	case EXPR_BINARY_LAZY_AND:     \
	case EXPR_BINARY_LAZY_OR:      \
	case EXPR_BINARY_AND:          \
	case EXPR_BINARY_OR:           \
	case EXPR_BINARY_XOR:          \
	case EXPR_BINARY_SHIFTLEFT:    \
	case EXPR_BINARY_SHIFTRIGHT:

/**
 * base structure for expressions
 */
struct expression_base_t {
	expression_kind_t  kind;
	type_t            *type;
	source_position_t  source_position;
	bool               lowered;
};

struct bool_const_t {
	expression_base_t base;
	bool              value;
};

struct int_const_t {
	expression_base_t base;
	int               value;
};

struct float_const_t {
	expression_base_t base;
	double            value;
};

struct string_const_t {
	expression_base_t  base;
	const char        *value;
};

struct func_expression_t {
	expression_base_t base;
	function_t        function;
};

struct reference_expression_t {
	expression_base_t  base;
	symbol_t          *symbol;
	entity_t          *entity;
	type_argument_t   *type_arguments;
};

struct call_argument_t {
	expression_t    *expression;
	call_argument_t *next;
};

struct call_expression_t {
	expression_base_t  base;
	expression_t      *function;
	call_argument_t   *arguments;
};

struct unary_expression_t {
	expression_base_t  base;
	expression_t      *value;
};

struct binary_expression_t {
	expression_base_t  base;
	expression_t      *left;
	expression_t      *right;
};

struct select_expression_t {
	expression_base_t  base;
	expression_t      *compound;
	symbol_t          *symbol;

	compound_entry_t *compound_entry;
	entity_t         *entity;
};

struct array_access_expression_t {
	expression_base_t  base;
	expression_t      *array_ref;
	expression_t      *index;
};

struct sizeof_expression_t {
	expression_base_t  base;
	type_t            *type;
};

union expression_t {
	expression_kind_t          kind;
	expression_base_t          base;
	bool_const_t               bool_const;
	int_const_t                int_const;
	float_const_t              float_const;
	string_const_t             string_const;
	func_expression_t          func;
	reference_expression_t     reference;
	call_expression_t          call;
	unary_expression_t         unary;
	binary_expression_t        binary;
	select_expression_t        select;
	array_access_expression_t  array_access;
	sizeof_expression_t        sizeofe;
};

typedef enum {
	STATEMENT_INVALID,
	STATEMENT_ERROR,
	STATEMENT_BLOCK,
	STATEMENT_RETURN,
	STATEMENT_DECLARATION,
	STATEMENT_IF,
	STATEMENT_EXPRESSION,
	STATEMENT_GOTO,
	STATEMENT_LABEL,
	STATEMENT_LAST = STATEMENT_LABEL
} statement_kind_t;

struct statement_base_t {
	statement_kind_t   kind;
	statement_t       *next;
	source_position_t  source_position;
};

struct return_statement_t {
	statement_base_t  base;
	expression_t     *value;
};

struct block_statement_t {
	statement_base_t   base;
	statement_t       *statements;
	source_position_t  end_position;
	context_t          context;
};

struct declaration_statement_t {
	statement_base_t base;
	variable_t       entity;
};

struct if_statement_t {
	statement_base_t  base;
	expression_t     *condition;
	statement_t      *true_statement;
	statement_t      *false_statement;
};

struct goto_statement_t {
	statement_base_t  base;
	symbol_t         *label_symbol;
	label_t          *label;
};

struct label_statement_t {
	statement_base_t base;
	label_t          label;
};

struct expression_statement_t {
	statement_base_t  base;
	expression_t     *expression;
};

union statement_t {
	statement_kind_t         kind;
	statement_base_t         base;
	return_statement_t       returns;
	block_statement_t        block;
	declaration_statement_t  declaration;
	goto_statement_t         gotos;
	label_statement_t        label;
	expression_statement_t   expression;
	if_statement_t           ifs;
};

struct concept_function_instance_t {
	function_t                   function;
	symbol_t                    *symbol;
	source_position_t            source_position;
	concept_function_instance_t *next;

	concept_function_t          *concept_function;
	concept_instance_t          *concept_instance;
};

struct concept_instance_t {
	symbol_t                    *concept_symbol;
	source_position_t            source_position;
	concept_t                   *concept;
	type_argument_t             *type_arguments;
	concept_function_instance_t *function_instances;
	concept_instance_t          *next;
	concept_instance_t          *next_in_concept;
	context_t                    context;
	type_variable_t             *type_parameters;
};

static inline void *_allocate_ast(size_t size)
{
	return obstack_alloc(&ast_obstack, size);
}

#define allocate_ast(size)                 _allocate_ast(size)

const char *get_entity_kind_name(entity_kind_t type);

/* ----- helpers for plugins ------ */

unsigned register_expression(void);
unsigned register_statement(void);
unsigned register_entity(void);

expression_t *allocate_expression(expression_kind_t kind);
entity_t *allocate_entity(entity_kind_t kind);

#endif
