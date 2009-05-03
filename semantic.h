#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "ast.h"

/* check static semantic of a bunch of files and organize them into modules
 * if semantic is fine */
bool check_semantic(void);

concept_instance_t *find_concept_instance(concept_t *concept);

concept_method_instance_t *get_method_from_concept_instance(
		concept_instance_t *instance, concept_method_t *method);

void init_semantic_module(void);
void exit_semantic_module(void);

#endif
