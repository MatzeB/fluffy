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

static
void parse_symbol(lexer_t *this, token_t *token)
{
	symbol_t *symbol;
	char     *string;

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

static
void parse_number_bin(lexer_t *this, token_t *token)
{
	next_char(this);
	if (this->c != '0' && this->c != '1') {
		fprintf(stderr,
			"Premature end of binary number literal at %s:%d\n",
			this->input_name, this->linenr);
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
				token->intvalue = value;
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
		fprintf(stderr,
			"Premature end of hex number literal at %s:%d\n",
			this->input_name, this->linenr);
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
			token->intvalue = value;
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
			token->intvalue = value;
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
			token->intvalue = value;
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
		if (this->c == 'b') { // binary number literal
			parse_number_bin(this, token);
		} else if (this->c == 'x' || this->c == 'X') { // hex number literal
			parse_number_hex(this, token);
		} else { // octal number literal
			parse_number_oct(this, token);
		}
	} else {
		parse_number_dec(this, token);
	}
}

static
void parse_string_literal(lexer_t *this, token_t *token)
{
	int         start_linenr = this->linenr;
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
			fprintf(stderr, "Error: unexpected end of file while parsing "
			        "string constant starting at %s:%d\n", this->input_name,
			        start_linenr);
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
	token->string   = result;
}

static
void skip_multiline_comment(lexer_t *this)
{
	int start_linenr = this->linenr;

	while(1) {
		if(this->c == '*') {
			next_char(this);
			if(this->c == '/') {
				next_char(this);
				return;
			}
		} else if(this->c == EOF) {
			fprintf(stderr, "Parse error: Comment starting at %s:%d not "
					"closed\n", this->input_name, start_linenr);
			return;
		} else {
			if(this->c == '\n') {
				this->linenr++;
			}
			next_char(this);
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

	while(isspace(this->c)) {
		if(this->c == '\n')
			this->linenr++;

		next_char(this);
	}

	token.sourcefile = this->input_name;
	token.linenr = this->linenr;

	switch(this->c) {
	case '(':
	case ')':
	case '{':
	case '}':
	case '[':
	case ']':
	case ',':
	case ';':
	case '+':
	case '-':
	case '*':
		token.type = this->c;
		next_char(this);
		return token;
	case '=':
		next_char(this);
		if(this->c == '=') {
			next_char(this);
			token.type = T_EQUALEQUAL;
		} else {
			token.type = '=';
		}
		return token;
	case '.':
		next_char(this);
		if(this->c == '.') {
			next_char(this);
			if(this->c == '.') {
				next_char(this);
				token.type = T_DOTDOTDOT;
			} else {
				token.type = T_DOTDOT;
			}
		} else {
			token.type = '.';
		}
		return token;
	case '/':
		next_char(this);
		if(this->c == '*') {
			next_char(this);
			skip_multiline_comment(this);
			return lexer_next_token(this);
		} else if(this->c == '/') {
			next_char(this);
			skip_line_comment(this);
			return lexer_next_token(this);
		} else if(this->c == '=') {
			next_char(this);
			token.type = T_SLASHEQUAL;
			return token;
		}
		token.type = this->c;
		return token;
	case '"':
		next_char(this);
		parse_string_literal(this, &token);
		return token;
	case '<':
		next_char(this);
		if(this->c == '-') {
			next_char(this);
			token.type = T_ASSIGN;
			return token;
		} else if(this->c == '=') {
			next_char(this);
			token.type = T_LESSEQUAL;
			return token;
		} else if(this->c == '<') {
			next_char(this);
			token.type = T_LESSLESS;
			return token;
		}
		token.type = '<';
		return token;
	case '>':
		next_char(this);
		if(this->c == '=') {
			next_char(this);
			token.type = T_GREATEREQUAL;
			return token;
		} else if(this->c == '>') {
			next_char(this);
			token.type = T_GREATERGREATER;
			return token;
		}
		token.type = '>';
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
	token.type = T_ERROR;
	next_char(this);
	return token;
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
}

void lexer_destroy(lexer_t *this)
{
	(void) this;
}

