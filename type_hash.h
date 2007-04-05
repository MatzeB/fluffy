#ifndef TYPE_HASH_H
#define TYPE_HASH_H

#include "ast.h"

void typehash_init(void);
void typehash_destroy(void);

type_t *typehash_insert(type_t *type);

#endif
