#ifndef MATCH_TYPE_H
#define MATCH_TYPE_H

#include "ast.h"
#include "semantic.h"
#include "lexer_t.h"

void match_variant_to_concrete_type(semantic_env_t *env,
                                    type_t *variant_type,
                                    type_t *concrete_type,
                                    const source_position_t source_position);

#endif

