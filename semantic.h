#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "ast.h"

typedef struct environment_entry_t  environment_entry_t;

int check_static_semantic(namespace_t *namespace);

typeclass_instance_t *find_typeclass_instance(typeclass_t *typeclass);

typeclass_method_instance_t *get_method_from_typeclass_instance(
		typeclass_instance_t *instance, typeclass_method_t *method);

void init_semantic_module(void);
void exit_semantic_module(void);

#endif
