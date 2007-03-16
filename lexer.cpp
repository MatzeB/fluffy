#include <stdio.h>

static FILE *input;
static char buf[1024];
static const char *bufend;
static const char *bufpos;
static const int c;

static INLINE void get_next_char()
{
	bufpos++;
	if(bufpos >= bufend) {
		size_t s = fread(buf, 1, sizeof(buf), input);
		if(s == 0) {
			c = EOF;
			return;
		}
		bufpos = buf;
		bufend = buf + s;
	}
	c = *bufpos;
}

void init_lexer(const char *fname)
{
	input = fopen(fname, "r");
	if(input == NULL) {
		fprintf(stderr, "Couldn't open '%s': %s\n", fname, strerror(errno));
		exit(1);
	}
	bufpos = NULL;
	bufend = NULL;
	get_next_char();
}

Token get_next_token()
{
	switch(c) {
	case '{':
	case '}':
	case '[':
	case ']':
	case '=':
	case ',':
	case '.':
		return c;
	}

	if(isalnum(c)) {
		return parse_identifiert();
	}
}

