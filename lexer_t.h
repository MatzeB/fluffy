#ifndef _LEXER_T_H_
#define _LEXER_T_H_

#include <stdio.h>
#include "symbol_table_t.h"
#include "adt/obst.h"

struct lexer_t {
	FILE *input;
	char buf[1024];
	const char *bufend;
	const char *bufpos;
	int c;
	symbol_table_t *symbol_table;
	struct obstack *obst;
	const char *input_name;
	unsigned linenr;
};

#include "lexer.h"

#endif

