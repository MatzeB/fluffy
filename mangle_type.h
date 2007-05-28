#ifndef MANGLE_TYPE_H
#define MANGLE_TYPE_H

#include "adt/obst.h"
#include "type.h"

/**
 * Pushes type mangled as string onto the obstack.
 */
void mangle_type(struct obstack *obst, const type_t *type);

#endif

