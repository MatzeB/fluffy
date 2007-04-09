#ifndef SYMBOL_H
#define SYMBOL_H

#include "ast.h"
#include "semantic.h"

typedef struct symbol_t symbol_t;

struct symbol_t {
	const char          *string;
	unsigned             ID;
	environment_entry_t *thing;
	environment_entry_t *label;
};

#endif
