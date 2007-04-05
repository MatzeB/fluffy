#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "symbol.h"

typedef struct symbol_table_t symbol_table_t;

symbol_t *symbol_table_insert(symbol_table_t *symbol_table, const char *symbol);

#endif
