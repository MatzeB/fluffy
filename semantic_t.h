#ifndef SEMANTIC_T_H
#define SEMANTIC_T_H

#include "semantic.h"

#include "ast.h"

struct entity_t {
	type_t *type;
	int     valnum;
	int     refs;
};

#endif

