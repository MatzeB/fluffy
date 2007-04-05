#ifndef LEXER_H
#define LEXER_H

#include "symbol_table_t.h"
#include "token_t.h"

typedef struct lexer_t lexer_t;

void lexer_init(lexer_t *lexer, symbol_table_t *symbol_table,
                FILE *stream, const char *input_name);

void lexer_destroy(lexer_t *lexer);

token_t lexer_next_token(lexer_t *lexer);

#endif
