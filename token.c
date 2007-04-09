#include <config.h>

#include "token_t.h"

#include <stdio.h>

void put_known_symbols_into_symbol_table(symbol_table_t *symbol_table)
{
	symbol_t *symbol;

#define T(x,str,val)                                 \
	symbol = symbol_table_insert(symbol_table, str); \
	symbol->ID = T_##x;
#include "known_symbols.inc"
#undef T
}

void print_token_type(FILE *f, token_type_t token_type)
{
	if(token_type >= 0 && token_type < 256) {
		fprintf(f, "'%c'", token_type);
		return;
	}

	switch(token_type) {
		case T_IDENTIFIER:     fputs("identifier",      f); break;
		case T_INTEGER:        fputs("integer number",  f); break;
		case T_STRING_LITERAL: fputs("string literal",  f); break;
		case T_EOF:            fputs("end of file",     f); break;
		case T_ERROR:          fputs("malformed token", f); break;

#define T(x,str,val) case T_##x: fputs("'" str "'", f); break;
#include "known_symbols.inc"
#undef T

		default: fputs("unknown token", f); break;
	}
}

void print_token(FILE *f, const token_t *token)
{
	switch(token->type) {
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
