#include <config.h>

#include "symbol_table.h"
#include "known_symbols.h"

void put_known_symbols_into_symbol_table(symbol_table_t *symbol_table)
{
	symbol_t *symbol;

#define T(x)	\
	symbol = symbol_table_insert(symbol_table, #x); \
	symbol->ID = T_##x;
#include "known_symbols.inc"
#undef T
#undef TOK2STR
}
