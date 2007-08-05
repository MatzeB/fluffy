#ifndef MATCH_TYPE_H
#define MATCH_TYPE_H

#include "semantic.h"
#include "type.h"
#include "lexer.h"

/**
 * compares a variant type (that contains 1 or more unbound type variable)
 * and a concrete and binds the type variables in the variant type so it
 * matches the concrete type
 */
void match_variant_to_concrete_type(type_t *variant_type,
                                    type_t *concrete_type,
                                    const source_position_t source_position);

#endif

