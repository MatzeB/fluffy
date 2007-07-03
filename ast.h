#ifndef AST_H
#define AST_H

#include <stdio.h>

typedef struct attribute_t              attribute_t;
typedef struct declaration_t            declaration_t;
typedef struct context_t                context_t;

typedef struct expression_t             expression_t;
typedef struct int_const_t              int_const_t;
typedef struct string_const_t           string_const_t;
typedef struct bool_const_t             bool_const_t;
typedef struct null_pointer_t           null_pointer_t;
typedef struct cast_expression_t        cast_expression_t;
typedef struct reference_expression_t   reference_expression_t;
typedef struct call_argument_t          call_argument_t;
typedef struct type_argument_t          type_argument_t;
typedef struct call_expression_t        call_expression_t;
typedef struct binary_expression_t      binary_expression_t;
typedef struct unary_expression_t       unary_expression_t;
typedef struct select_expression_t      select_expression_t;
typedef struct array_access_expression_t array_access_expression_t;
typedef struct sizeof_expression_t      sizeof_expression_t;
typedef struct func_expression_t        func_expression_t;

typedef struct statement_t              statement_t;
typedef struct block_statement_t        block_statement_t;
typedef struct return_statement_t       return_statement_t;
typedef struct if_statement_t           if_statement_t;
typedef struct variable_declaration_t   variable_declaration_t;
typedef struct variable_declaration_statement_t
                                        variable_declaration_statement_t;
typedef struct expression_statement_t   expression_statement_t;
typedef struct goto_statement_t         goto_statement_t;
typedef struct label_declaration_t      label_declaration_t;
typedef struct label_statement_t        label_statement_t;

typedef struct namespace_t              namespace_t;

typedef struct method_parameter_t       method_parameter_t;
typedef struct method_t                 method_t;
typedef struct method_declaration_t     method_declaration_t;
typedef struct constant_t               constant_t;
typedef struct global_variable_t        global_variable_t;
typedef struct typealias_t              typealias_t;
typedef struct typeclass_instance_t     typeclass_instance_t;
typedef struct typeclass_method_instance_t
                                        typeclass_method_instance_t;
typedef struct typeclass_t              typeclass_t;
typedef struct typeclass_method_t       typeclass_method_t;

void  init_ast_module(void);
void  exit_ast_module(void);

void  print_ast(FILE *out, const namespace_t *namespace);
void *allocate_ast(size_t size);

#endif
