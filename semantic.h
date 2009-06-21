#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "ast.h"

/* check static semantic of a bunch of files and organize them into modules
 * if semantic is fine */
bool check_semantic(void);

concept_instance_t *find_concept_instance(concept_t *concept);

concept_function_instance_t *get_function_from_concept_instance(
		concept_instance_t *instance, concept_function_t *function);

void init_semantic_module(void);
void exit_semantic_module(void);

#endif
