#ifndef LEXER_H
#define LEXER_H

#include "symbol_table_t.h"
#include "token_t.h"

typedef struct source_position_t source_position_t;
struct source_position_t {
	const char *input_name;
	unsigned    linenr;
};
extern source_position_t source_position;

void lexer_init(FILE *stream, const char *input_name);
void lexer_destroy(void);

void lexer_next_token(token_t *token);

#endif
