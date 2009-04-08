#ifndef MANGE_H
#define MANGE_H

#include "ast.h"
#include <libfirm/firm_types.h>

ident *create_name_linux_elf(declaration_t *decl);
ident *create_name_macho(declaration_t *decl);
ident *create_name_win32(declaration_t *decl);

void init_mangle(void);
void exit_mangle(void);

#endif

