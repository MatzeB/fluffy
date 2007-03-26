#ifndef _SYMBOL_H_
#define _SYMBOL_H_

#include "ast.h"

typedef struct {
	const char *string;
	unsigned ID;
	/* additional stuff */
	environment_entry_t *thing;
} symbol_t;

#endif
