#include <config.h>

#include "lexer_t.h"
#include "symbol_table_t.h"

#include <errno.h>
#include <string.h>
#include <ctype.h>

enum TOKEN_START_TYPE {
	START_UNKNOWN = 0,
	START_IDENT,
	START_NUMBER,
	START_SINGLE_CHARACTER_OPERATOR,
	START_OPERATOR,
	START_STRING_LITERAL,
	START_CHARACTER_CONSTANT
};

static int  tables_init = 0;
static char char_type[256];
static char ident_char[256];

static
void init_tables()
{
	memset(char_type, 0, sizeof(char_type));
	for(int c = 0; c <= 256; ++c) {
		if(isalnum(c)) {
			char_type[c]  = START_IDENT;
			ident_char[c] = 1;
		}
	}
	char_type['_']  = START_IDENT;
	ident_char['_'] = 1;
	for(int c = '0'; c <= '9'; ++c) {
		ident_char[c] = 1;
		char_type[c]  = START_NUMBER;
	}

	char_type['"']  = START_STRING_LITERAL;
	char_type['\''] = START_CHARACTER_CONSTANT;

	static const int single_char_ops[] = {
		'(', ')', '[', ']', '{', '}', ',', ':', ';'
	};
	for(size_t i = 0; i < sizeof(single_char_ops)/sizeof(single_char_ops[0]);
	    ++i) {
		int c        = single_char_ops[i];
		char_type[c] = START_SINGLE_CHARACTER_OPERATOR;
	}

	static const int ops[] = {
		'+', '-', '*', '/', '=', '<', '>', '.', '^', '!', '?', '&', '%',
		'~', '|'
	};
	for(size_t i = 0; i < sizeof(ops)/sizeof(ops[0]); ++i) {
		int c        = ops[i];
		char_type[c] = START_OPERATOR;
	}

	tables_init = 1;
}

static inline
int is_ident_char(int c)
{
	return ident_char[c];
}

static
void error_prefix_at(lexer_t *this, const char *input_name, unsigned linenr)
{
	(void) this;
	fprintf(stderr, "%s:%d: Error: ", input_name, linenr);
}

static
void error_prefix(lexer_t *this)
{
	error_prefix_at(this, this->input_name, this->linenr);
}

static
void parse_error(lexer_t *this, const char *msg)
{
	error_prefix(this);
	fprintf(stderr, "%s\n", msg);
}

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

static
void parse_symbol(lexer_t *this, token_t *token)
{
	symbol_t *symbol;
	char     *string;

	do {
		obstack_1grow(this->obst, this->c);
		next_char(this);
	} while(is_ident_char(this->c));
	obstack_1grow(this->obst, '\0');
	string = obstack_finish(this->obst);

	symbol = symbol_table_insert(this->symbol_table, string);

	if(symbol->ID > 0) {
		token->type = symbol->ID;
	} else {
		token->type = T_IDENTIFIER;
	}
	token->v.symbol = symbol;

	if(symbol->string != string) {
		obstack_free(this->obst, string);
	}
}

static
void parse_number_bin(lexer_t *this, token_t *token)
{
	next_char(this);
	if (this->c != '0' && this->c != '1') {
		parse_error(this, "premature end of binary number literal");
		token->type = T_ERROR;
		return;
	}

	int value = 0;
	for(;;) {
		switch (this->c) {
			case '0': value = 2 * value;     break;
			case '1': value = 2 * value + 1; break;

			default:
				token->type     = T_INTEGER;
				token->v.intvalue = value;
				return;
		}
		next_char(this);
	}
}

static
void parse_number_hex(lexer_t *this, token_t *token)
{
	next_char(this);
	if (!isdigit(this->c) &&
		!('A' <= this->c && this->c <= 'F') &&
		!('a' <= this->c && this->c <= 'f')) {
		parse_error(this, "premature end of hex number literal");
		token->type = T_ERROR;
		return;
	}

	int value = 0;
	for(;;) {
		if (isdigit(this->c)) {
			value = 16 * value + this->c - '0';
		} else if ('A' <= this->c && this->c <= 'F') {
			value = 16 * value + this->c - 'A' + 10;
		} else if ('a' <= this->c && this->c <= 'f') {
			value = 16 * value + this->c - 'f' + 10;
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char(this);
	}
}

static
void parse_number_oct(lexer_t *this, token_t *token)
{
	int value = 0;
	for(;;) {
		if ('0' <= this->c && this->c <= '7') {
			value = 8 * value + this->c - '0';
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char(this);
	}
}

static
void parse_number_dec(lexer_t *this, token_t *token)
{
	int value = 0;
	for(;;) {
		if (isdigit(this->c)) {
			value = 10 * value + this->c - '0';
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char(this);
	}
}

static
void parse_number(lexer_t *this, token_t *token)
{
	// TODO check for overflow
	// TODO check for various invalid inputs sequences

	if (this->c == '0') {
		next_char(this);
		switch (this->c) {
			case 'b': parse_number_bin(this, token); break;
			case 'X':
			case 'x': parse_number_hex(this, token); break;
			default:  parse_number_oct(this, token); break;
		}
	} else {
		parse_number_dec(this, token);
	}
}

static
void parse_string_literal(lexer_t *this, token_t *token)
{
	unsigned    start_linenr = this->linenr;
	char       *string;
	const char *result;
	int         c = this->c;

	while(c != '\"') {
		if(c == '\\') {
			next_char(this);
			switch(this->c) {
			case 'a': c = '\a'; break;
			case 'b': c = '\b'; break;
			case 'f': c = '\f'; break;
			case 'n': c = '\n';	break;
			case 'r': c = '\r'; break;
			case 't': c = '\t'; break;
			case 'v': c = '\v'; break;
			case '\\': c = '\\'; break;
			case '"': c = '"'; break;
			case '\'': c = '\''; break;
			case '?': c = '?'; break;
			case 'x': /* TODO parse hex number ... */
				fprintf(stderr, "Warning: Hex escape sequences not implemented yet\n");
				c = 'x';
				break;
			case 0:
			case 1:
			case 2:
			case 3:
			case 4:
			case 5:
			case 6:
			case 7:
				fprintf(stderr, "Warning: oktal parsing not implemented yet\n");
				c = this->c;
				break;
			case EOF:
				c = EOF;
				break;
			default:
				fprintf(stderr, "Warning: Unknown escape sequence \\%c\n",
				        this->c);
				c = this->c;
				break;
			}
		} else if(c == '\n') {
			this->linenr++;
		}

		if(c == EOF) {
			error_prefix_at(this, this->input_name, start_linenr);
			fprintf(stderr, "string has no end\n");
			token->type = T_ERROR;
			return;
		}

		obstack_1grow(this->obst, c);
		next_char(this);
		c = this->c;
	}
	next_char(this);

	/* add finishing 0 to the string */
	obstack_1grow(this->obst, '\0');
	string = obstack_finish(this->obst);

	/* check if there is already a copy of the string */
	result = strset_insert(&this->stringset, string);
	if(result != string) {
		obstack_free(this->obst, string);
	}

	token->type     = T_STRING_LITERAL;
	token->v.string = result;
}

static
void skip_multiline_comment(lexer_t *this)
{
	unsigned start_linenr = this->linenr;

	while(1) {
		if(this->c == '*') {
			next_char(this);
			if(this->c == '/') {
				next_char(this);
				return;
			}
		} else if(this->c == EOF) {
			error_prefix_at(this, this->input_name, start_linenr);
			fprintf(stderr, "comment has no end\n");
			return;
		} else {
			if(this->c == '\n') {
				this->linenr++;
			}
			next_char(this);
		}
	}
}

static 
void skip_line_comment(lexer_t *this)
{
	while(this->c != '\n' && this->c != EOF) {
		next_char(this);
	}
}

static
void parse_operator(lexer_t *this, token_t *token)
{
	do {
		obstack_1grow(this->obst, this->c);
		next_char(this);
	} while(char_type[this->c] == START_OPERATOR);
	obstack_1grow(this->obst, '\0');
	char     *string = obstack_finish(this->obst);
	symbol_t *symbol = symbol_table_insert(this->symbol_table, string);

	int ID = symbol->ID;
	if(ID > 0) {
		if(ID == T_MULTILINE_COMMENT_BEGIN) {
			skip_multiline_comment(this);
			return lexer_next_token(this, token);
		} else if(ID == T_SINGLELINE_COMMENT) {
			skip_line_comment(this);
			return lexer_next_token(this, token);
		} else {
			token->type = symbol->ID;
		}
	} else {
		error_prefix(this);
		fprintf(stderr, "unknown operator %s found\n", string);
		token->type = T_ERROR;
	}
	token->v.symbol = symbol;

	if(symbol->string != string) {
		obstack_free(this->obst, string);
	}
}

void lexer_next_token(lexer_t *this, token_t *token)
{
	/* skip whitespaces */
	while(isspace(this->c)) {
		if(this->c == '\n')
			this->linenr++;

		next_char(this);
	}

	token->sourcefile = this->input_name;
	token->linenr = this->linenr;

	if(this->c < 0 || this->c >= 256) {
		token->type = T_EOF;
		return;
	}

	int type = char_type[this->c];
	switch(type) {
	case START_SINGLE_CHARACTER_OPERATOR:
		token->type = this->c;
		next_char(this);
		break;

	case START_OPERATOR:
		parse_operator(this, token);
		break;

	case START_IDENT:
		parse_symbol(this, token);
		break;

	case START_NUMBER:
		parse_number(this, token);
		break;

	case START_STRING_LITERAL:
		parse_string_literal(this, token);
		break;

	case START_CHARACTER_CONSTANT:
		/* TODO */
		abort();
		break;

	default:
		error_prefix(this);
		fprintf(stderr, "unknown character '%c' found\n", this->c);
		token->type = T_ERROR;
		next_char(this);
		break;
	}
}

void lexer_init(lexer_t *this, symbol_table_t *symbol_table,
                FILE *stream, const char *input_name)
{
	this->input  = stream;
	this->bufpos = NULL;
	this->bufend = NULL;
	next_char(this);

	this->symbol_table = symbol_table;
	this->obst         = &symbol_table->obst;
	this->linenr       = 1;
	this->input_name   = input_name;
	strset_init(&this->stringset);

	if(!tables_init) {
		init_tables();
	}
}

void lexer_destroy(lexer_t *this)
{
	(void) this;
}
