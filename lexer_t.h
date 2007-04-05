#ifndef LEXER_T_H
#define LEXER_T_H

#include "lexer.h"

#include <stdio.h>
#include "symbol_table_t.h"
#include "adt/obst.h"
#include "adt/strset.h"

struct lexer_t {
	int             c;
	FILE           *input;
	char            buf[1024];
	const char     *bufend;
	const char     *bufpos;
	symbol_table_t *symbol_table;
	struct obstack *obst;
	const char     *input_name;
	unsigned        linenr;
	strset_t        stringset;
};

#endif
