#ifndef MANGE_H
#define MANGE_H

#include "ast.h"
#include "symbol.h"
#include "type.h"
#include <libfirm/firm_types.h>

void start_mangle(void);
void mangle_symbol_simple(symbol_t *symbol);
void mangle_symbol(symbol_t *symbol);
void mangle_concept_name(symbol_t *symbol);
ident *finish_mangle(void);
void mangle_type(const type_t *type);

void init_mangle(void);
void exit_mangle(void);

#endif

