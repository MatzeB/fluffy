#ifndef AST_H
#define AST_H

#include <stdio.h>

typedef struct type_t                   type_t;
typedef struct atomic_type_t            atomic_type_t;
typedef struct type_reference_t         type_reference_t;
typedef struct struct_entry_t           struct_entry_t;
typedef struct struct_type_t            struct_type_t;
typedef struct type_constraint_t        type_constraint_t;
typedef struct type_variable_t          type_variable_t;
typedef struct method_parameter_type_t  method_parameter_type_t;
typedef struct method_type_t            method_type_t;
typedef struct pointer_type_t           pointer_type_t;

typedef struct expression_t             expression_t;
typedef struct int_const_t              int_const_t;
typedef struct cast_expression_t        cast_expression_t;
typedef struct reference_expression_t   reference_expression_t;
typedef struct call_argument_t          call_argument_t;
typedef struct type_argument_t          type_argument_t;
typedef struct call_expression_t        call_expression_t;
typedef struct binary_expression_t      binary_expression_t;
typedef struct unary_expression_t       unary_expression_t;
typedef struct select_expression_t      select_expression_t;
typedef struct sizeof_expression_t      sizeof_expression_t;

typedef struct statement_t              statement_t;
typedef struct block_statement_t        block_statement_t;
typedef struct return_statement_t       return_statement_t;
typedef struct if_statement_t           if_statement_t;
typedef struct variable_declaration_statement_t
                                        variable_declaration_statement_t;
typedef struct expression_statement_t   expression_statement_t;
typedef struct goto_statement_t         goto_statement_t;
typedef struct label_statement_t        label_statement_t;

typedef enum   namespace_entry_type_t   namespace_entry_type_t;
typedef struct namespace_entry_t        namespace_entry_t;
typedef struct namespace_t              namespace_t;
typedef struct method_parameter_t       method_parameter_t;
typedef struct method_t                 method_t;
typedef struct extern_method_t          extern_method_t;
typedef struct variable_t               variable_t;
typedef struct struct_t                 struct_t;
typedef struct typeclass_instance_t     typeclass_instance_t;
typedef struct typeclass_method_instance_t
                                        typeclass_method_instance_t;
typedef struct typeclass_t              typeclass_t;
typedef struct typeclass_method_t       typeclass_method_t;

extern type_t *type_void;
extern type_t *type_invalid;

void print_type(FILE* out, const type_t *type);
void print_expression(FILE *out, const expression_t *expression);

int type_valid(const type_t *type);

#endif
