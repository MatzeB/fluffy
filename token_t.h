#ifndef _TOKEN_T_H_
#define _TOKEN_T_H_

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

#endif

