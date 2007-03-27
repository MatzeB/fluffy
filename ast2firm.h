#ifndef _AST2FIRM_H_
#define _AST2FIRM_H_

#include "ast.h"

void ast2firm(compilation_unit_t *unit);

void initialize_firm(void);
void exit_firm(void);

#endif

