#ifndef _TOKEN_T_H_
#define _TOKEN_T_H_

typedef enum token_type_t {
	TOKEN_SYMBOL   = 256,
	TOKEN_INTEGER,
	TOKEN_EOF
} token_type_t;

typedef struct token_t {
	int type;
	const char *sourcefile;
	unsigned linenr;
	union {
		symbol_table_entry_t *symbol;
		int intvalue;
	};
} token_t;

#endif

