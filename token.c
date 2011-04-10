#include <config.h>

#include "token_t.h"

#include <assert.h>
#include <stdio.h>

#include "symbol.h"
#include "adt/array.h"

static symbol_t **token_symbols = NULL;

void init_tokens(void)
{
	symbol_t *symbol;

	token_symbols = NEW_ARR_F(symbol_t*, T_LAST_TOKEN);
	memset(token_symbols, 0, T_LAST_TOKEN * sizeof(token_symbols[0]));

#define T(x,str,val)                                               \
	assert(T_##x >= 0 && T_##x < T_LAST_TOKEN);                    \
	symbol               = symbol_table_insert(str);               \
	symbol->ID           = T_##x;                                  \
	token_symbols[T_##x] = symbol;

#define TS(x,str,val)                                              \
	assert(T_##x >= 0 && T_##x < T_LAST_TOKEN);                    \
	symbol               = symbol_table_insert(str);               \
	token_symbols[T_##x] = symbol;

#include "tokens.inc"

#undef TS
#undef T
}

void exit_tokens(void)
{
	DEL_ARR_F(token_symbols);
	token_symbols = NULL;
}

int register_new_token(const char *token)
{
	int token_id = ARR_LEN(token_symbols);
	
	symbol_t *symbol = symbol_table_insert(token);
	symbol->ID       = token_id;
	ARR_APP1(symbol_t*, token_symbols, symbol);

	return token_id;
}

void print_token_type(FILE *f, token_type_t token_type)
{
	if (token_type < 256) {
		fprintf(f, "'%c'", token_type);
		return;
	}
	if (token_type == T_EOF) {
		fputs("end of file", f);
		return;
	}

	unsigned token_symbols_len = (unsigned)ARR_LEN(token_symbols);
	if (token_type >= token_symbols_len) {
		fputs("invalid token", f);
		return;
	}

	const symbol_t *symbol = token_symbols[token_type];
	if (symbol != NULL) {
		fputs(symbol->string, f);
	} else {
		fputs("unknown token", f);
	}
}

void print_token(FILE *f, const token_t *token)
{
	switch (token->type) {
	case T_IDENTIFIER:
		fprintf(f, "symbol '%s'", token->v.symbol->string);
		break;
	case T_INTEGER:
		fprintf(f, "integer number %d", token->v.intvalue);
		break;
	case T_STRING_LITERAL:
		fprintf(f, "string '%s'", token->v.string);
		break;
	default:
		print_token_type(f, token->type);
		break;
	}
}

