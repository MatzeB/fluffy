#ifndef AST2FIRM_H
#define AST2FIRM_H

#include <libfirm/firm_types.h>
#include "ast.h"

void ast2firm(const module_t *modules);

ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos);
unsigned dbg_snprint(char *buf, unsigned len, const dbg_info *dbg);
const char *dbg_retrieve(const dbg_info *dbg, unsigned *line);

void init_ast2firm(void);
void exit_ast2firm(void);

#endif

