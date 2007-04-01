#ifndef _AST_H_
#define _AST_H_

typedef struct type_t                   type_t;
typedef struct atomic_type_t            atomic_type_t;
typedef struct ref_type_t               ref_type_t;

typedef struct expression_t             expression_t;
typedef struct int_const_t              int_const_t;
typedef struct cast_expression_t        cast_expression_t;

typedef struct statement_t              statement_t;
typedef struct block_statement_t        block_statement_t;
typedef struct return_statement_t       return_statement_t;

typedef struct compilation_unit_t       compilation_unit_t;

typedef struct function_t               function_t;
typedef struct variable_t               variable_t;
typedef struct argument_t               argument_t;
typedef enum   environment_entry_type_t environment_entry_type_t;
typedef struct environment_entry_t      environment_entry_t;
typedef struct environment_t            environment_t;

void push_environment(environment_t *environment);

void pop_environment(environment_t *environment);

#endif
