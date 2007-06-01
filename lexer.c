#include <config.h>

#include "lexer_t.h"
#include "symbol_table_t.h"
#include "adt/error.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#define MAX_PUTBACK 1

enum TOKEN_START_TYPE {
	START_UNKNOWN = 0,
	START_IDENT,
	START_NUMBER,
	START_SINGLE_CHARACTER_OPERATOR,
	START_OPERATOR,
	START_STRING_LITERAL,
	START_CHARACTER_CONSTANT,
	START_BACKSLASH,
	START_NEWLINE,
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
		'~', '|', '\\'
	};
	for(size_t i = 0; i < sizeof(ops)/sizeof(ops[0]); ++i) {
		int c        = ops[i];
		char_type[c] = START_OPERATOR;
	}

	char_type['\n'] = START_NEWLINE;
	char_type['\\'] = START_BACKSLASH;

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
	error_prefix_at(this, this->source_position.input_name,
	                this->source_position.linenr);
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
		size_t s = fread(this->buf + MAX_PUTBACK, 1,
		                 sizeof(this->buf) - MAX_PUTBACK, this->input);
		if(s == 0) {
			this->c = EOF;
			return;
		}
		this->bufpos = this->buf + MAX_PUTBACK;
		this->bufend = this->buf + MAX_PUTBACK + s;
	}
	this->c = *(this->bufpos);
}

static inline
void put_back(lexer_t *this, int c)
{
	char *p = (char*) this->bufpos - 1;
	this->bufpos--;
	assert(p >= this->buf);
	*p = c;
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
	assert(this->c == 'b' || this->c == 'B');
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
	assert(this->c == 'x' || this->c == 'X');
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
			value = 16 * value + this->c - 'a' + 10;
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
	assert(this->c == 'o' || this->c == 'O');
	next_char(this);

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
void parse_number_dec(lexer_t *this, token_t *token, int first_char)
{
	int value = 0;
	if(first_char > 0) {
		assert(first_char >= '0' && first_char <= '9');
		value = first_char - '0';
	}

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
			case 'b':
			case 'B': parse_number_bin(this, token); break;
			case 'X':
			case 'x': parse_number_hex(this, token); break;
			case 'o':
			case 'O': parse_number_oct(this, token); break;
			default:  parse_number_dec(this, token, '0');
		}
	} else {
		parse_number_dec(this, token, 0);
	}
}

static
int parse_escape_sequence(lexer_t *this)
{
	assert(this->c == '\\');
	next_char(this);

	int c = this->c;
	next_char(this);

	switch(c) {
	case 'a': return '\a';
	case 'b': return '\b';
	case 'f': return '\f';
	case 'n': return '\n';
	case 'r': return '\r';
	case 't': return '\t';
	case 'v': return '\v';
	case '\\': return '\\';
	case '"': return '"';
	case '\'': return'\'';
	case '?': return '\?';
	case 'x': /* TODO parse hex number ... */
		parse_error(this, "hex escape sequences not implemented yet");
		return EOF;
	case 'o': /* TODO parse octal number ... */
		parse_error(this, "octal escape sequences not implemented yet");
		return EOF;
	case EOF:
		parse_error(this, "reached end of file while parsing escape sequence");
		return EOF;
	default:
		parse_error(this, "unknown escape sequence\n");
		return EOF;
	}
}

static
void parse_string_literal(lexer_t *this, token_t *token)
{
	unsigned    start_linenr = this->source_position.linenr;
	char       *string;
	const char *result;

	assert(this->c == '"');
	next_char(this);

	int c = this->c;
	while(c != '\"') {
		if(c == '\\') {
			c = parse_escape_sequence(this);
		} else {
			if(c == '\n') {
				this->source_position.linenr++;
			}
			next_char(this);
		}

		if(c == EOF) {
			error_prefix_at(this, this->source_position.input_name,
			                start_linenr);
			fprintf(stderr, "string has no end\n");
			token->type = T_ERROR;
			return;
		}

		obstack_1grow(this->obst, c);
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
	unsigned start_linenr = this->source_position.linenr;

	while(1) {
		if(this->c == '*') {
			next_char(this);
			if(this->c == '/') {
				next_char(this);
				return;
			}
		} else if(this->c == EOF) {
			error_prefix_at(this, this->source_position.input_name,
			                start_linenr);
			fprintf(stderr, "comment has no end\n");
			return;
		} else {
			if(this->c == '\n') {
				this->source_position.linenr++;
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
void parse_operator(lexer_t *this, token_t *token, int firstchar)
{
	if(firstchar == '/') {
		if(this->c == '*') {
			next_char(this);
			skip_multiline_comment(this);
			return lexer_next_token(this, token);
		} else if(this->c == '/') {
			next_char(this);
			skip_line_comment(this);
			return lexer_next_token(this, token);
		}
	}

	obstack_1grow(this->obst, firstchar);
	while(char_type[this->c] == START_OPERATOR) {
		obstack_1grow(this->obst, this->c);
		next_char(this);
	}
	obstack_1grow(this->obst, '\0');
	char     *string = obstack_finish(this->obst);
	symbol_t *symbol = symbol_table_insert(this->symbol_table, string);

	int ID = symbol->ID;
	if(ID > 0) {
		token->type = ID;
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

static
void parse_indent(lexer_t *this, token_t *token)
{
	if(this->not_returned_dedents > 0) {
		token->type = T_DEDENT;
		this->not_returned_dedents--;
		if(this->not_returned_dedents == 0)
			this->at_line_begin = 0;
		return;
	}

	char     indent[MAX_INDENT];
	unsigned indent_len = 0;
	while(this->c == ' ' || this->c == '\t') {
		indent[indent_len] = this->c;
		indent_len++;
		if(indent_len > MAX_INDENT) {
			panic("Indentation bigger than MAX_INDENT not supported");
		}
		next_char(this);
	}

	/* skip empty lines */
	while(this->c == '/') {
		next_char(this);
		if(this->c == '*') {
			next_char(this);
			skip_multiline_comment(this);
		} else if(this->c == '/') {
			next_char(this);
			skip_line_comment(this);
		} else {
			put_back(this, this->c);
		}
	}
	if(this->c == '\n') {
		next_char(this);
		this->source_position.linenr++;
		lexer_next_token(this, token);
		return;
	}
	this->at_line_begin = 0;

	unsigned i;
	for(i = 0; i < indent_len && i < this->last_line_indent_len; ++i) {
		if(indent[i] != this->last_line_indent[i]) {
			parse_error(this, "space/tab usage for indentation different from "
			            "previous line");
			token->type = T_ERROR;
			return;
		}
	}
	if(this->last_line_indent_len < indent_len) {
		/* more indentation */
		memcpy(& this->last_line_indent[i], & indent[i], indent_len - i);
		this->last_line_indent_len = indent_len;

		this->indent_levels[this->indent_levels_len] = indent_len;
		this->indent_levels_len++;

		token->type = T_INDENT;
		return;
	} else if(this->last_line_indent_len > indent_len) {
		/* less indentation */
		unsigned lower_level;
		unsigned dedents = 0;
		do {
			dedents++;
			this->indent_levels_len--;
			lower_level = this->indent_levels[this->indent_levels_len - 1];
		} while(lower_level > indent_len);

		if(lower_level < indent_len) {
			parse_error(this, "returning to invalid indentation level");
			token->type = T_ERROR;
			return;
		}
		assert(dedents >= 1);
		this->not_returned_dedents  = dedents - 1;
		if(this->not_returned_dedents > 0)
			this->at_line_begin = 1;

		this->last_line_indent_len = indent_len;

		token->type = T_DEDENT;
		return;
	}

	lexer_next_token(this, token);
	return;
}

void lexer_next_token(lexer_t *this, token_t *token)
{
	int firstchar;

	if(this->at_line_begin) {
		parse_indent(this, token);
		return;
	} else {
		/* skip whitespaces */
		while(this->c == ' ' || this->c == '\t') {
			next_char(this);
		}
	}

	if(this->c < 0 || this->c >= 256) {
		/* if we're indented at end of file, then emit a newline, dedent, ...
		 * sequence of tokens */
		if(this->indent_levels_len > 1) {
			this->not_returned_dedents = this->indent_levels_len - 1;
			this->at_line_begin        = 1;
			this->indent_levels_len    = 1;
			token->type                = T_NEWLINE;
			return;
		}
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
		firstchar = this->c;
		next_char(this);
		parse_operator(this, token, firstchar);
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
		next_char(this);
		if(this->c == '\\') {
			token->type       = T_INTEGER;
			token->v.intvalue = parse_escape_sequence(this);
		} else {
			if(this->c == '\n') {
				parse_error(this, "newline while parsing character constant");
				this->source_position.linenr++;
			}
			token->type       = T_INTEGER;
			token->v.intvalue = this->c;
		}
		next_char(this);
		if(this->c != '\'') {
			parse_error(this, "multibyte character constant");
			token->type = T_ERROR;
		}
		break;

	case START_NEWLINE:
		next_char(this);
		token->type = T_NEWLINE;
		this->source_position.linenr++;
		this->at_line_begin = 1;
		break;

	case START_BACKSLASH:
		next_char(this);
		if(this->c == '\n') {
			next_char(this);
			this->source_position.linenr++;
		} else {
			parse_operator(this, token, '\\');
			return;
		}
		lexer_next_token(this, token);
		return;

	default:
		error_prefix(this);
		fprintf(stderr, "unknown character '%c' found\n", this->c);
		token->type = T_ERROR;
		next_char(this);
		break;
	}
}

/* hack for now... */
lexer_t *current_lexer = NULL;

void lexer_init(lexer_t *this, symbol_table_t *symbol_table,
                FILE *stream, const char *input_name)
{
	memset(this, 0, sizeof(this[0]));

	this->input = stream;

	this->symbol_table               = symbol_table;
	this->obst                       = &symbol_table->obst;
	this->source_position.linenr     = 1;
	this->source_position.input_name = input_name;
	this->at_line_begin              = 1;
	this->indent_levels[0]           = 0;
	this->indent_levels_len          = 1;
	strset_init(&this->stringset);

	if(!tables_init) {
		init_tables();
	}

	next_char(this);

	current_lexer = this;
}

void lexer_destroy(lexer_t *this)
{
	(void) this;
}

static __attribute__((unused))
void dbg_pos(const source_position_t source_position)
{
	fprintf(stdout, "%s:%d\n", source_position.input_name, source_position.linenr);
	fflush(stdout);
}

