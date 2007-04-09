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
	char            ident_table[128];
	char            operator_table[128];
};

void lexer_init(lexer_t *lexer, symbol_table_t *symbol_table,
                FILE *stream, const char *input_name);

void lexer_destroy(lexer_t *lexer);

#endif
