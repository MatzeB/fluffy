#ifndef LEXER_T_H
#define LEXER_T_H

#include "lexer.h"

#include <stdio.h>
#include "adt/obst.h"
#include "adt/strset.h"

#define MAX_INDENT               256

typedef struct source_position_t source_position_t;
struct source_position_t {
	const char *input_name;
	unsigned    linenr;
};

struct lexer_t {
	int               c;
	source_position_t source_position;
	FILE             *input;
	char              buf[1024];
	const char       *bufend;
	const char       *bufpos;
	strset_t          stringset;
	int               at_line_begin;
	unsigned          not_returned_dedents;
	unsigned          indent_levels[MAX_INDENT];
	unsigned          indent_levels_len;
	char              last_line_indent[MAX_INDENT];
	unsigned          last_line_indent_len;
};

void lexer_init(lexer_t *lexer, FILE *stream, const char *input_name);

void lexer_destroy(lexer_t *lexer);

#endif
