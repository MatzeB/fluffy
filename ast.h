#ifndef AST_H
#define AST_H

#include <stdbool.h>
#include <stdio.h>

typedef struct attribute_t              attribute_t;
typedef union  entity_t                 entity_t;
typedef union  expression_t             expression_t;
typedef union  statement_t              statement_t;

typedef struct context_t                context_t;
typedef struct export_t                 export_t;
typedef struct import_t                 import_t;
typedef struct expression_base_t        expression_base_t;
typedef struct int_const_t              int_const_t;
typedef struct float_const_t            float_const_t;
typedef struct string_const_t           string_const_t;
typedef struct bool_const_t             bool_const_t;
typedef struct cast_expression_t        cast_expression_t;
typedef struct reference_expression_t   reference_expression_t;
typedef struct call_argument_t          call_argument_t;
typedef struct call_expression_t        call_expression_t;
typedef struct binary_expression_t      binary_expression_t;
typedef struct unary_expression_t       unary_expression_t;
typedef struct select_expression_t      select_expression_t;
typedef struct array_access_expression_t array_access_expression_t;
typedef struct sizeof_expression_t      sizeof_expression_t;
typedef struct func_expression_t        func_expression_t;

typedef struct statement_base_t         statement_base_t;
typedef struct block_statement_t        block_statement_t;
typedef struct return_statement_t       return_statement_t;
typedef struct if_statement_t           if_statement_t;
typedef struct declaration_statement_t  declaration_statement_t;
typedef struct expression_statement_t   expression_statement_t;
typedef struct goto_statement_t         goto_statement_t;
typedef struct label_t                  label_t;
typedef struct label_statement_t        label_statement_t;

typedef struct module_t                 module_t;

typedef struct function_parameter_t     function_parameter_t;
typedef struct function_t               function_t;
typedef struct variable_t               variable_t;
typedef struct function_entity_t        function_entity_t;
typedef struct constant_t               constant_t;
typedef struct global_variable_t        global_variable_t;
typedef struct typealias_t              typealias_t;
typedef struct concept_instance_t       concept_instance_t;
typedef struct concept_function_instance_t
                                        concept_function_instance_t;
typedef struct concept_t                concept_t;
typedef struct concept_function_t       concept_function_t;

void  init_ast_module(void);
void  exit_ast_module(void);

void  print_ast(FILE *out, const context_t *context);
void  print_expression(const expression_t *expression);
void *allocate_ast(size_t size);

/**
 * Returns true if a given expression is a compile time
 * constant.
 */
bool is_constant_expression(const expression_t *expression);

/**
 * An object with a fixed but at compiletime unknown adress which will be known
 * at link/load time.
 */
bool is_linktime_constant(const expression_t *expression);

long fold_constant_to_int(expression_t *expression);
bool fold_constant_to_bool(expression_t *expression);

#endif
