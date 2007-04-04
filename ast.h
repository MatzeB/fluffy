#ifndef _AST_H_
#define _AST_H_

typedef struct type_t                   type_t;
typedef struct atomic_type_t            atomic_type_t;
typedef struct ref_type_t               ref_type_t;

typedef struct expression_t             expression_t;
typedef struct int_const_t              int_const_t;
typedef struct cast_expression_t        cast_expression_t;
typedef struct variable_reference_expression_t variable_reference_expression_t;
typedef struct call_expression_t        call_expression_t;
typedef struct assign_expression_t      assign_expression_t;

typedef struct statement_t              statement_t;
typedef struct block_statement_t        block_statement_t;
typedef struct return_statement_t       return_statement_t;
typedef struct variable_declaration_statement_t variable_declaration_statement_t;
typedef struct expression_statement_t   expression_statement_t;

typedef enum   namespace_entry_type_t   namespace_entry_type_t;
typedef struct namespace_entry_t        namespace_entry_t;
typedef struct namespace_t              namespace_t;
typedef struct function_t               function_t;
typedef struct variable_t               variable_t;
typedef struct extern_function_t        extern_function_t;

#endif
