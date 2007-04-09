#ifndef TOKEN_T_H
#define TOKEN_T_H

#include <stdio.h>
#include "symbol.h"
#include "symbol_table.h"

typedef enum {
	T_NEWLINE = 256,
	T_INDENT,
	T_DEDENT,
	T_IDENTIFIER,
	T_INTEGER,
	T_STRING_LITERAL,
#define T(x,str,val) T_##x val,
#include "known_symbols.inc"
#undef T
	T_EOF      = -1,
	T_ERROR    = -2
} token_type_t;

typedef struct {
	int type;
	union {
		symbol_t   *symbol;
		int         intvalue;
		const char *string;
	} v;
} token_t;

void put_known_symbols_into_symbol_table(symbol_table_t *symbol_table);
void print_token_type(FILE *out, token_type_t token_type);
void print_token(FILE *out, const token_t *token);

#endif
