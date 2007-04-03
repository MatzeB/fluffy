#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "ast.h"

typedef struct entity_t            entity_t;
typedef struct environment_entry_t environment_entry_t;

int check_static_semantic(namespace_t *namespace);

#endif

