#include <config.h>

#include "token_t.h"

#include <stdio.h>

#include "known_symbols.h"

void print_token_type(FILE *f, token_type_t token_type)
{
	if(token_type < 256) {
		fprintf(f, "'%c'", token_type);
		return;
	} 

	switch(token_type) {
	case T_EQUALEQUAL:
		fputs("'=='", f);
		break;
	case T_EXCLAMATIONEQUAL:
		fputs("'!='", f);
		break;
	case T_IDENTIFIER:
		fprintf(f, "identifier");
		break;
	case T_INTEGER:
		fprintf(f, "integer number");
		break;
	case T_EOF:
		fprintf(f, "end of file");
		break;
#define T(x)                                  \
	case T_##x:                               \
		fprintf(f, "'" #x "'");               \
		break;
#include "known_symbols.inc"
#undef T
	default:
		fprintf(f, "unknown token");
		break;
	}
}

void print_token(FILE *f, const token_t *token)
{
	if(token->type == T_IDENTIFIER) {
		fprintf(f, "symbol '%s'", token->symbol->string);
		return;
	}
	if(token->type == T_INTEGER) {
		fprintf(f, "integer number %d", token->intvalue);
		return;
	}

	print_token_type(f, token->type);
}
