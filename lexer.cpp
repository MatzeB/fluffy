#include <stdio.h>

static FILE *input;
static char buf[1024];
static const char *bufend;
static const char *bufpos;
static const int c;
static struct obstack obst;

static inline
void get_next_char()
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
	struct obstack lexer_obst;

	obst_init(&obst);

	input = fopen(fname, "r");
	if(input == NULL) {
		fprintf(stderr, "Couldn't open '%s': %s\n", fname, strerror(errno));
		exit(1);
	}
	bufpos = NULL;
	bufend = NULL;
	get_next_char();
}

void destroy_lexer()
{
	fclose(input);
	obstack_free(&obst, NULL);
}

Token parse_identifier()
{
	Token token;

	do {
		obstack_1grow(&obst, c);
		get_next_char();
	} while(isalnum(c));

	token.ID = TOKEN_ID_IDENTIFIER;
	token.sourcefile = NULL;
	token.linenr = 0;
	token.identifier = 
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

	if(isalpha(c)) {
		return parse_identifier();
	}
}

