#ifndef AST2FIRM_H
#define AST2FIRM_H

#include "ast.h"

void ast2firm(namespace_t *unit);

void initialize_firm(void);
void exit_firm(void);

#endif
