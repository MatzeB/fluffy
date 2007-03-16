#ifndef TOKEN_H_
#define TOKEN_H_

struct Token
{
	int ID;
	const char *sourcefile;
	unsigned linenr;
};

#endif

