#ifndef AST2FIRM_H
#define AST2FIRM_H

#include <libfirm/firm_types.h>
#include "ast.h"

void ast2firm(void);
ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos);

void init_ast2firm(void);
void exit_ast2firm(void);

#endif

