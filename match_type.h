#ifndef MATCH_TYPE_H
#define MATCH_TYPE_H

#include "semantic.h"
#include "type.h"
#include "lexer.h"

void match_variant_to_concrete_type(type_t *variant_type,
                                    type_t *concrete_type,
                                    const source_position_t source_position);

#endif

