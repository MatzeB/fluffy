/**  
 * @file type.h 
 *
 * Project:     libFIRM                                                   <br>
 * File name:   ir/tr/type.h                                              <br>
 * Purpose:     Representation of types.                                  <br>
 * Author:      Goetz Lindenmaier                                         <br>
 * Modified by:                                                           <br>
 * Created:                                                               <br>
 * Copyright:   (c) 2001-2003 Universit�t Karlsruhe                       <br>
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE. <br>
 * CVS-ID:      $Id$         
 *
 *
 */

# ifndef _TYPE_IDENTIFY_H_
# define _TYPE_IDENTIFY_H_

#include "firm_types.h"

/* ------------------------------------------------------------------------ */

/**  Type for a function that compares two types. 
 *  
 *   @param tp1  The first type to compare.
 *   @param tp2  The second type to compare.
 */
typedef int (compare_types_func_t)(const void *tp1, const void *tp2);

/** Compares two types by their name. 
 *
 * Compares the opcode and the name of the types. If these are 
 * equal returns 0, else non-zero. 
 */
int compare_names (const void *tp1, const void *tp2);

/** Compares two types strict. 
 *
 * returns 0 if tp1 == tp2, else non-zero
 */
int compare_strict (const void *tp1, const void *tp2);

/* ------------------------------------------------------------------------ */

/**  Type for a function that computes a hash value for a type. 
 *  
 *   @param tp The type to compute a hash for. 
 */
typedef int (hash_types_func_t)(ir_type *tp);

/** Computes a hash value by the type name. 
 *
 * Uses the name of the type and the type opcode to compute the hash. 
 */
int firm_hash_name (ir_type *tp);

/* ------------------------------------------------------------------------ */

/** Finalize type construction. 
 * 
 * Indicate that a type is so far completed that it can be
 * distinguished from other types.  Mature_type hashes the type into a
 * table.  It uses the function in compare_types_func to compare the
 * types.
 *
 * If it finds a type identical to tp it returns this type.  It turns
 * tp into the Id type.  All places formerly pointing to tp will now
 * point to the found type.  All entities of tp now refer to the found
 * type as their owner, but they are not a member of this type.  This
 * is invalid firm -- the entities must be replaced by entities of the
 * found type.  The Id type will be removed from the representation
 * automatically, but within an unknown time span.  It occupies memory
 * for this time.
 *
 * @param tp     The type to mature.  
 */
ir_type *    mature_type(ir_type *tp);

/** Finalize type construction. 
 * 
 * Indicate that a type is so far completed that it can be
 * distinguished from other types.  Mature_type hashes the type into a
 * table.  It uses the function in compare_types_func to compare the
 * types.
 *
 * If it finds a type identical to tp it returns this type.  It frees
 * type tp and all its entities.
 *
 * @param tp     The type to mature.  
 */
ir_type *    mature_type_free(ir_type *tp);

/** Finalize type construction. 
 * 
 * Indicate that a type is so far completed that it can be
 * distinguished from other types.  Mature_type hashes the type into a
 * table.  It uses the function in compare_types_func to compare the
 * types.  
 *
 * If it find a type identical to tp it returns this type.  It frees
 * the entities and turns the type into an Id type.  All places
 * formerly pointing to tp will now point to the found type.  The Id
 * type will be removed from the representation automatically, but
 * within an unknown time span.  It occupies memory for this time.
 *
 * @param tp     The type to mature.  
 */
ir_type *    mature_type_free_entities(ir_type *tp);

/**
 * The interface type for the type identify module;
 */
typedef struct _type_identify_if_t {
  compare_types_func_t *cmp;    /**< The function that should be used to compare two types.
                                     If NULL, compare_strict() will be used. */
  hash_types_func_t *hash;      /**< The function that should be used to calculate a hash
                                     value of a type. If NULL, hash_name() will be used. */
} type_identify_if_t;

/**
 * Initialise the type identifier module.
 *
 * @param ti_if    The interface functions for this module.
 *
 * If the parameter ti_if is NULL, the default functions compare_strict() and
 * firm_hash_name() will be used.
 */
void init_type_identify(type_identify_if_t *ti_if);

# endif /* _TYPE_IDENTIFY_H_ */
