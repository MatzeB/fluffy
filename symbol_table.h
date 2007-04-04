#ifndef _SYMBOL_TABLE_H_
#define _SYMBOL_TABLE_H_

#include "symbol.h"

typedef struct symbol_table_t symbol_table_t;

symbol_t *symbol_table_insert(symbol_table_t *symbol_table, const char *symbol);

#endif

