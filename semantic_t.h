#ifndef SEMANTIC_T_H
#define SEMANTIC_T_H

#include "compiler.h"
#include "semantic.h"
#include "ast.h"
#include "type.h"
#include "adt/obst.h"
#include "symbol.h"
#include "lexer_t.h"

typedef statement_t* (*lower_statement_function) (statement_t *statement);
typedef expression_t* (*lower_expression_function) (expression_t *expression);

void print_error_prefix(const source_position_t position);
void print_warning_prefix(const source_position_t position);
void error_at(const source_position_t position, const char *message);

void register_statement_lowerer(lower_statement_function function,
                                unsigned int statement_type);

void register_expression_lowerer(lower_expression_function function,
                                 unsigned int expression_type);

WARN_UNUSED
statement_t* check_statement(statement_t *statement);

WARN_UNUSED
expression_t* check_expression(expression_t *expression);

#endif

