#ifndef TYPE_H
#define TYPE_H

#include <stdio.h>

typedef struct type_t                   type_t;
typedef struct atomic_type_t            atomic_type_t;
typedef struct type_reference_t         type_reference_t;
typedef struct compound_entry_t         compound_entry_t;
typedef struct compound_type_t          compound_type_t;
typedef struct type_constraint_t        type_constraint_t;
typedef struct type_variable_t          type_variable_t;
typedef struct method_parameter_type_t  method_parameter_type_t;
typedef struct method_type_t            method_type_t;
typedef struct pointer_type_t           pointer_type_t;

extern type_t *type_void;
extern type_t *type_invalid;

void init_type_module(void);
void exit_type_module(void);

/**
 * prints a human readable form of @p type to a stream
 */
void print_type(FILE* out, const type_t *type);

/**
 * returns 1 if type contains integer numbers
 */
int is_type_int(const type_t *type);

/**
 * returns 1 if the type is valid. A type is valid if it contains no unresolved
 * references anymore and is not of TYPE_INVALID.
 */
int type_valid(const type_t *type);

/**
 * returns a normalized copy of a type with type variables replaced by their
 * current type. The given type (and all its hierarchy) is not modified.
 */
type_t *create_concrete_type(type_t *type);

#endif

