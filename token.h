#ifndef TOKEN_H_
#define TOKEN_H_

#define TOKEN_ID_IDENTIFIER		256
#define TOKEN_ID_INTEGER        257

struct Token
{
	int ID;
	const char *sourcefile;
	unsigned linenr;
	union {
		const char *identifier;
		int intvalue;
	};
};

#endif

