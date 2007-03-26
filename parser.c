#include <config.h>

#include "parser.h"

#include <stdio.h>

#include "symbol_table_t.h"
#include "known_symbols.h"
#include "lexer_t.h"
#include "symbol.h"
#include "adt/obst.h"

typedef struct {
	struct obstack obst;
	token_t token;
	lexer_t lexer;
	symbol_table_t symbol_table;
} parser_env_t;

static
void dump_token(token_t token)
{
	printf("Found Token: %d", token.type);
	if(token.type == T_IDENTIFIER) {
		printf(" (identifier '%s'-%p)", token.symbol->string, token.symbol->string);
	} else if(token.type >= FIRST_KNOWN_SYMBOL) {
		printf(" (symbol ID %d string '%s'-%p)", token.symbol->ID, token.symbol->string, token.symbol->string);
	} else if(token.type < 256 && token.type >= 0) {
		printf(" '%c'", token.type);
	}
	printf("\n");
}

static inline
void next_token(parser_env_t* env)
{
	env->token = lexer_next_token(&env->lexer);
}

void parse(FILE *in)
{
	parser_env_t env;

	obstack_init(&env.obst);

	symbol_table_init(&env.symbol_table);
	put_known_symbols_into_symbol_table(&env.symbol_table);

	lexer_init(&env.lexer, &env.symbol_table, in);

	do {
		next_token(&env);
		dump_token(env.token);
	} while(env.token.type != T_EOF);

	lexer_destroy(&env.lexer);
	symbol_table_destroy(&env.symbol_table);

	obstack_free(&env.obst, NULL);
}

