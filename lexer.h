#ifndef _LEXER_H_
#define _LEXER_H_

#include "symbol_table_t.h"
#include "token_t.h"

typedef struct lexer_t lexer_t;

void lexer_init(lexer_t *lexer, symbol_table_t *symbol_table,
                FILE *stream);

void lexer_destroy(lexer_t *lexer);

token_t lexer_next_token(lexer_t *lexer);

#endif

