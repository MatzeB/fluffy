#include <config.h>

#include "lexer.h"
#include "symbol_table.h"
#include "adt/error.h"
#include "adt/strset.h"
#include "adt/array.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#define MAX_PUTBACK  1
#define MAX_INDENT   256

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

static int         c;
source_position_t  source_position;
static FILE       *input;
static char        buf[1024 + MAX_PUTBACK];
static const char *bufend;
static const char *bufpos;
static strset_t    stringset;
static int         at_line_begin;
static unsigned    not_returned_dedents;
static unsigned    newline_after_dedents;
static unsigned    indent_levels[MAX_INDENT];
static unsigned    indent_levels_len;
static char        last_line_indent[MAX_INDENT];
static unsigned    last_line_indent_len;


static
void init_tables(void)
{
	memset(char_type, 0, sizeof(char_type));
	memset(ident_char, 0, sizeof(ident_char));
	for(int c = 0; c < 256; ++c) {
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
		'(', ')', '[', ']', '{', '}', ',', ':', ';', '*'
	};
	for(size_t i = 0; i < sizeof(single_char_ops)/sizeof(single_char_ops[0]);
	    ++i) {
		int c        = single_char_ops[i];
		char_type[c] = START_SINGLE_CHARACTER_OPERATOR;
	}

	static const int ops[] = {
		'+', '-', '/', '=', '<', '>', '.', '^', '!', '?', '&', '%',
		'~', '|', '\\', '$'
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
void error_prefix_at(const char *input_name, unsigned linenr)
{
	fprintf(stderr, "%s:%d: Error: ", input_name, linenr);
}

static
void error_prefix(void)
{
	error_prefix_at(source_position.input_name, source_position.linenr);
}

static
void parse_error(const char *msg)
{
	error_prefix();
	fprintf(stderr, "%s\n", msg);
}

static inline
void next_char(void)
{
	bufpos++;
	if(bufpos >= bufend) {
		size_t s = fread(buf + MAX_PUTBACK, 1, sizeof(buf) - MAX_PUTBACK,
		                 input);
		if(s == 0) {
			c = EOF;
			return;
		}
		bufpos = buf + MAX_PUTBACK;
		bufend = buf + MAX_PUTBACK + s;
	}
	c = *(bufpos);
}

static inline
void put_back(int c)
{
	char *p = (char*) bufpos - 1;
	bufpos--;
	assert(p >= buf);
	*p = c;
}

static
void parse_symbol(token_t *token)
{
	symbol_t *symbol;
	char     *string;

	do {
		obstack_1grow(&symbol_obstack, c);
		next_char();
	} while(is_ident_char(c));
	obstack_1grow(&symbol_obstack, '\0');
	string = obstack_finish(&symbol_obstack);

	symbol = symbol_table_insert(string);

	if(symbol->ID > 0) {
		token->type = symbol->ID;
	} else {
		token->type = T_IDENTIFIER;
	}
	token->v.symbol = symbol;

	if(symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
}

static
void parse_number_bin(token_t *token)
{
	assert(c == 'b' || c == 'B');
	next_char();

	if (c != '0' && c != '1') {
		parse_error("premature end of binary number literal");
		token->type = T_ERROR;
		return;
	}

	int value = 0;
	for(;;) {
		switch (c) {
			case '0': value = 2 * value;     break;
			case '1': value = 2 * value + 1; break;

			default:
				token->type     = T_INTEGER;
				token->v.intvalue = value;
				return;
		}
		next_char();
	}
}

static
void parse_number_hex(token_t *token)
{
	assert(c == 'x' || c == 'X');
	next_char();

	if (!isdigit(c) &&
		!('A' <= c && c <= 'F') &&
		!('a' <= c && c <= 'f')) {
		parse_error("premature end of hex number literal");
		token->type = T_ERROR;
		return;
	}

	int value = 0;
	for(;;) {
		if (isdigit(c)) {
			value = 16 * value + c - '0';
		} else if ('A' <= c && c <= 'F') {
			value = 16 * value + c - 'A' + 10;
		} else if ('a' <= c && c <= 'f') {
			value = 16 * value + c - 'a' + 10;
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char();
	}
}

static
void parse_number_oct(token_t *token)
{
	assert(c == 'o' || c == 'O');
	next_char();

	int value = 0;
	for(;;) {
		if ('0' <= c && c <= '7') {
			value = 8 * value + c - '0';
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char();
	}
}

static
void parse_number_dec(token_t *token, int first_char)
{
	int value = 0;
	if(first_char > 0) {
		assert(first_char >= '0' && first_char <= '9');
		value = first_char - '0';
	}

	for(;;) {
		if (isdigit(c)) {
			value = 10 * value + c - '0';
		} else {
			token->type     = T_INTEGER;
			token->v.intvalue = value;
			return;
		}
		next_char();
	}
}

static
void parse_number(token_t *token)
{
	// TODO check for overflow
	// TODO check for various invalid inputs sequences

	if (c == '0') {
		next_char();
		switch (c) {
			case 'b':
			case 'B': parse_number_bin(token); break;
			case 'X':
			case 'x': parse_number_hex(token); break;
			case 'o':
			case 'O': parse_number_oct(token); break;
			default:  parse_number_dec(token, '0');
		}
	} else {
		parse_number_dec(token, 0);
	}
}

static
int parse_escape_sequence(void)
{
	assert(c == '\\');
	next_char();

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
		parse_error("hex escape sequences not implemented yet");
		return 0;
	case 'o': /* TODO parse octal number ... */
		parse_error("octal escape sequences not implemented yet");
		return 0;
	case EOF:
		parse_error("reached end of file while parsing escape sequence");
		return EOF;
	default:
		parse_error("unknown escape sequence\n");
		return 0;
	}
}

static
void parse_string_literal(token_t *token)
{
	unsigned    start_linenr = source_position.linenr;
	char       *string;
	const char *result;

	assert(c == '"');
	next_char();

	while(c != '\"') {
		if(c == '\\') {
			c = parse_escape_sequence();
		} else if(c == '\n') {
			source_position.linenr++;
		}

		if(c == EOF) {
			error_prefix_at(source_position.input_name, start_linenr);
			fprintf(stderr, "string has no end\n");
			token->type = T_ERROR;
			return;
		}

		obstack_1grow(&symbol_obstack, c);
		next_char();
	}
	next_char();

	/* add finishing 0 to the string */
	obstack_1grow(&symbol_obstack, '\0');
	string = obstack_finish(&symbol_obstack);

	/* check if there is already a copy of the string */
	result = strset_insert(&stringset, string);
	if(result != string) {
		obstack_free(&symbol_obstack, string);
	}

	token->type     = T_STRING_LITERAL;
	token->v.string = result;
}

static
void skip_multiline_comment(void)
{
	unsigned start_linenr = source_position.linenr;
	unsigned level = 1;

	while(1) {
		switch(c) {
		case '*':
			next_char();
			if(c == '/') {
				next_char();
				level--;
				if(level == 0)
					return;
			}
			break;
		case '/':
			next_char();
			if(c == '*') {
				next_char();
				level++;
			}
			break;
		case EOF:
			error_prefix_at(source_position.input_name, start_linenr);
			fprintf(stderr, "comment has no end\n");
			return;
		case '\n':
			next_char();
			source_position.linenr++;
			break;
		default:
			next_char();
			break;
		}
	}
}

static 
void skip_line_comment(void)
{
	while(c != '\n' && c != EOF) {
		next_char();
	}
}

static
void parse_operator(token_t *token, int firstchar)
{
	if(firstchar == '/') {
		if(c == '*') {
			next_char();
			skip_multiline_comment();
			return lexer_next_token(token);
		} else if(c == '/') {
			next_char();
			skip_line_comment();
			return lexer_next_token(token);
		}
	}

	obstack_1grow(&symbol_obstack, firstchar);
	while(char_type[c] == START_OPERATOR) {
		obstack_1grow(&symbol_obstack, c);
		next_char();
	}
	obstack_1grow(&symbol_obstack, '\0');
	char     *string = obstack_finish(&symbol_obstack);
	symbol_t *symbol = symbol_table_insert(string);

	int ID = symbol->ID;
	if(ID > 0) {
		token->type = ID;
	} else {
		error_prefix();
		fprintf(stderr, "unknown operator %s found\n", string);
		token->type = T_ERROR;
	}
	token->v.symbol = symbol;

	if(symbol->string != string) {
		obstack_free(&symbol_obstack, string);
	}
}

static
void parse_indent(token_t *token)
{
	if(not_returned_dedents > 0) {
		token->type = T_DEDENT;
		not_returned_dedents--;
		if(not_returned_dedents == 0 && !newline_after_dedents)
			at_line_begin = 0;
		return;
	}

	if(newline_after_dedents) {
		token->type = T_NEWLINE;
		at_line_begin         = 0;
		newline_after_dedents = 0;
		return;
	}

	int      skipped_line = 0;
	char     indent[MAX_INDENT];
	unsigned indent_len;

start_indent_parsing:
	indent_len = 0;
	while(c == ' ' || c == '\t') {
		indent[indent_len] = c;
		indent_len++;
		if(indent_len > MAX_INDENT) {
			panic("Indentation bigger than MAX_INDENT not supported");
		}
		next_char();
	}

	/* skip empty lines */
	while(c == '/') {
		next_char();
		if(c == '*') {
			next_char();
			skip_multiline_comment();
		} else if(c == '/') {
			next_char();
			skip_line_comment();
		} else {
			put_back(c);
		}
	}
	if(c == '\n') {
		next_char();
		source_position.linenr++;
		skipped_line = 1;
		goto start_indent_parsing;
	}
	at_line_begin = 0;

	unsigned i;
	for(i = 0; i < indent_len && i < last_line_indent_len; ++i) {
		if(indent[i] != last_line_indent[i]) {
			parse_error("space/tab usage for indentation different from "
			            "previous line");
			token->type = T_ERROR;
			return;
		}
	}
	if(last_line_indent_len < indent_len) {
		/* more indentation */
		memcpy(& last_line_indent[i], & indent[i], indent_len - i);
		last_line_indent_len  = indent_len;
		newline_after_dedents = 0;

		indent_levels[indent_levels_len] = indent_len;
		indent_levels_len++;

		token->type = T_INDENT;
		return;
	} else if(last_line_indent_len > indent_len) {
		/* less indentation */
		unsigned lower_level;
		unsigned dedents = 0;
		do {
			dedents++;
			indent_levels_len--;
			lower_level = indent_levels[indent_levels_len - 1];
		} while(lower_level > indent_len);

		if(lower_level < indent_len) {
			parse_error("returning to invalid indentation level");
			token->type = T_ERROR;
			return;
		}
		assert(dedents >= 1);

		not_returned_dedents = dedents - 1;
		if(skipped_line) {
			newline_after_dedents = 1;
			at_line_begin         = 1;
		} else {
			newline_after_dedents = 0;
			if(not_returned_dedents > 0) {
				at_line_begin = 1;
			}
		}

		last_line_indent_len = indent_len;

		token->type = T_DEDENT;
		return;
	}

	lexer_next_token(token);
	return;
}

void lexer_next_token(token_t *token)
{
	int firstchar;

	if(at_line_begin) {
		parse_indent(token);
		return;
	} else {
		/* skip whitespaces */
		while(c == ' ' || c == '\t') {
			next_char();
		}
	}

	if(c < 0 || c >= 256) {
		/* if we're indented at end of file, then emit a newline, dedent, ...
		 * sequence of tokens */
		if(indent_levels_len > 1) {
			not_returned_dedents = indent_levels_len - 1;
			at_line_begin        = 1;
			indent_levels_len    = 1;
			token->type          = T_NEWLINE;
			return;
		}
		token->type = T_EOF;
		return;
	}

	int type = char_type[c];
	switch(type) {
	case START_SINGLE_CHARACTER_OPERATOR:
		token->type = c;
		next_char();
		break;

	case START_OPERATOR:
		firstchar = c;
		next_char();
		parse_operator(token, firstchar);
		break;

	case START_IDENT:
		parse_symbol(token);
		break;

	case START_NUMBER:
		parse_number(token);
		break;

	case START_STRING_LITERAL:
		parse_string_literal(token);
		break;

	case START_CHARACTER_CONSTANT:
		next_char();
		if(c == '\\') {
			token->type       = T_INTEGER;
			token->v.intvalue = parse_escape_sequence();
			next_char();
		} else {
			if(c == '\n') {
				parse_error("newline while parsing character constant");
				source_position.linenr++;
			}
			token->type       = T_INTEGER;
			token->v.intvalue = c;
			next_char();
		}

		{
			int err_displayed = 0;
			while(c != '\'' && c != EOF) {
				if(!err_displayed) {
					parse_error("multibyte character constant");
					err_displayed = 1;
				}
				token->type = T_ERROR;
				next_char();
			}
		}
		next_char();
		break;

	case START_NEWLINE:
		next_char();
		token->type = T_NEWLINE;
		source_position.linenr++;
		at_line_begin = 1;
		break;

	case START_BACKSLASH:
		next_char();
		if(c == '\n') {
			next_char();
			source_position.linenr++;
		} else {
			parse_operator(token, '\\');
			return;
		}
		lexer_next_token(token);
		return;

	default:
		error_prefix();
		fprintf(stderr, "unknown character '%c' found\n", c);
		token->type = T_ERROR;
		next_char();
		break;
	}
}

void lexer_init(FILE *stream, const char *input_name)
{
	input                      = stream;
	bufpos                     = NULL;
	bufend                     = NULL;
	source_position.linenr     = 1;
	source_position.input_name = input_name;
	at_line_begin              = 1;
	indent_levels[0]           = 0;
	indent_levels_len          = 1;
	last_line_indent_len       = 0;
	strset_init(&stringset);
	not_returned_dedents       = 0;
	newline_after_dedents      = 0;

	if(!tables_init) {
		init_tables();
	}

	next_char();
}

void lexer_destroy(void)
{
}

static __attribute__((unused))
void dbg_pos(const source_position_t source_position)
{
	fprintf(stdout, "%s:%d\n", source_position.input_name,
	                           source_position.linenr);
	fflush(stdout);
}

