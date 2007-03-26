#ifndef _SYMBOL_TABLE_H_
#define _SYMBOL_TABLE_H_

#include "symbol.h"

#ifndef _SYMBOL_TABLE_T_H_
typedef struct symbol_table_t symbol_table_t;
#endif

void symbol_table_init(symbol_table_t *symbol_table);

void symbol_table_destroy(symbol_table_t *symbol_table);

symbol_t *symbol_table_insert(symbol_table_t *symbol_table, const char *symbol);

#endif

