#include <config.h>

#include "lexer_t.h"
#include "symbol_table_t.h"

#include <errno.h>
#include <string.h>
#include <ctype.h>

static inline
void next_char(lexer_t *this)
{
	this->bufpos++;
	if(this->bufpos >= this->bufend) {
		size_t s = fread(this->buf, 1, sizeof(this->buf), this->input);
		if(s == 0) {
			this->c = EOF;
			return;
		}
		this->bufpos = this->buf;
		this->bufend = this->buf + s;
	}
	this->c = *(this->bufpos);
}

static inline
void parse_symbol(lexer_t *this, token_t *token)
{
	symbol_t *symbol;
	char *string;

	do {
		obstack_1grow(this->obst, this->c);
		next_char(this);
	} while(isalnum(this->c) || this->c == '_');
	obstack_1grow(this->obst, '\0');
	string = obstack_finish(this->obst);

	symbol = symbol_table_insert(this->symbol_table, string);

	if(symbol->ID > 0) {
		token->type = symbol->ID;
	} else {
		token->type = T_IDENTIFIER;
	}
	token->symbol = symbol;

	if(symbol->string != string) {
		obstack_free(this->obst, string);
	}
}

static inline
void parse_number(lexer_t *this, token_t *token)
{
	char *string;

	do {
		obstack_1grow(this->obst, this->c);
		next_char(this);
	} while(isdigit(this->c));
	obstack_1grow(this->obst, '\0');
	string = obstack_finish(this->obst);

	token->type = T_INTEGER;
	token->intvalue = atoi(string);
	obstack_free(this->obst, string);
}

static inline
void skip_mutiline_comment(lexer_t *this)
{
	while(1) {
		if(this->c == '*') {
			next_char(this);
			if(this->c == '/') {
				next_char(this);
				return;
			}
		} else if(this->c == EOF) {
			fprintf(stderr, "Parse error: Comment starting at line 'TODO' not closed\n");
			return;
		}
	}
}

static inline
void skip_line_comment(lexer_t *this)
{
	while(this->c != '\n' && this->c != EOF) {
		next_char(this);
	}
}

token_t lexer_next_token(lexer_t *this)
{
	token_t token;
	token.sourcefile = "TODO";
	token.linenr = 666;

	while(isspace(this->c)) {
		next_char(this);
	}

	switch(this->c) {
	case '(':
	case ')':
	case '{':
	case '}':
	case '[':
	case ']':
	case '=':
	case ',':
	case '.':
	case ';':
		token.type = this->c;
		next_char(this);
		return token;
	case '/':
		next_char(this);
		if(this->c == '*') {
			next_char(this);
			skip_mutiline_comment(this);
			return lexer_next_token(this);
		} else if(this->c == '/') {
			next_char(this);
			skip_line_comment(this);
			return lexer_next_token(this);
		}
		token.type = this->c;
		return token;
	case EOF:
		token.type = T_EOF;
		return token;
	}

	if(isalpha(this->c) || this->c == '_') {
		parse_symbol(this, &token);
		return token;
	}

	if(isdigit(this->c)) {
		parse_number(this, &token);
		return token;
	}

	fprintf(stderr, "Unexpected character '%c' found.\n", this->c);
	next_char(this);
	return lexer_next_token(this);
}

void lexer_init(lexer_t *this, symbol_table_t *symbol_table,
                FILE *stream)
{
	this->input = stream;
	this->bufpos = NULL;
	this->bufend = NULL;
	next_char(this);

	this->symbol_table = symbol_table;
	this->obst = & symbol_table->obst;
}

void lexer_destroy(lexer_t *this)
{
	(void) this;
}

