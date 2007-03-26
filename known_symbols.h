#ifndef _KNOWN_SYMBOLS_H_
#define _KNOWN_SYMBOLS_H_

#include "token_t.h"
#include "symbol_table.h"

enum TokenId {
	LAST_CHAR_TOKEN = FIRST_KNOWN_SYMBOL - 1,
#define T(x) T_##x,
#include "known_symbols.inc"
#undef T
	T_LAST
};

void put_known_symbols_into_symbol_table(symbol_table_t *symbol_table);

#endif

	
