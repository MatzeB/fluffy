#ifndef _TOKEN_T_H_
#define _TOKEN_T_H_

#include <stdio.h>
#include "symbol.h"

typedef enum {
	T_IDENTIFIER = 256,
	T_INTEGER,
	FIRST_KNOWN_SYMBOL,
	T_EOF      = -1
} token_type_t;

typedef struct {
	int type;
	const char *sourcefile;
	unsigned linenr;
	union {
		symbol_t *symbol;
		int intvalue;
	};
} token_t;

void print_token_type(FILE *out, token_type_t token_type);
void print_token(FILE *out, const token_t *token);

#endif

