/*
 * Project:     firmlower
 * File name:   src/pseudo_op.h
 * Purpose:     Pseudo firm operations Catch and Bounds.
 * Author:      Till Riedel
 * Modified by:
 * Created:     03.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe. All rights reserved.
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef __PSEUDO_OP_H
#define __PSEUDO_OP_H

#include <libfirm/irnode.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Firm operation Catch: Catch an exception and return the exception object. 
 */
typedef enum {
  pn_Catch_M            = pn_Call_M_regular,
  pn_Catch_T_result     = pn_Call_T_result,
  pn_Catch_T_result_res = 0,
  pn_Catch__arity
} pn_Catch;

/**
 * Creates a new Catch pseudo operation.
 *
 * @param db         debug info
 * @param irg        the ir_graph were the op is inserted
 * @param b          the block were to insert
 * @param store      the current store
 * @param exc_type   the return type of the Catch operation
 */
ir_node *new_rd_Catch(dbg_info *db, ir_graph *irg, ir_node *b, ir_node *store, ir_type *exc_type);

/**
 * Creates a new Catch pseudo operation.
 *
 * @param db         debug info
 * @param store      the current store
 * @param exc_type   the return type of the Catch operation
 */
ir_node *new_d_Catch(dbg_info *db, ir_node *store, ir_type *exc_type);

#define new_Catch(s, x)         new_d_Catch (NULL,       s, x)
#define new_r_Catch(g, b, s, x) new_rd_Catch(NULL, g, b, s, x)

extern ident * pseudo_Catch_id;

/** Returns non-zero if a given node is a Catch pseudo operation. */
int      is_Catch(ir_node *);

/** Returns the memory input of a Catch operation. */
ir_node *get_Catch_mem(ir_node *);

/** Returns the type of the Catch return value. */
ir_type *get_Catch_exo_type(ir_node *);

void     opt_Catch(ir_node *);


/** Firm operation Bounds:  Array bounds check. 
 *
 * Performs bounds check given an array index, and lower and upper bound 
 * of the array.  Throws an exception if index is not within the bounds. 
 */

typedef enum {
  pn_Bounds_M = pn_Call_M_regular,
  pn_Bounds_X = pn_Call_X_except,
} pn_Bounds;

/**
 * Creates a new Bound pseudo operation.
 *
 * @param db         debug info
 * @param irg        the ir_graph were the op is inserted
 * @param b          the block were to insert
 * @param store      the current store
 * @param index      a node representing the accessed index
 * @param low        a node representing the lower array bound
 * @param high       a node representing the upper array bound
 */
ir_node *new_rd_Bounds(dbg_info *db, ir_graph *irg, ir_node *b, ir_node *store, 
		       ir_node *index, ir_node *low, ir_node *high);

/**
 * Creates a new Bound pseudo operation.
 *
 * @param db         debug info
 * @param store      the current store
 * @param index      a node representing the accessed index
 * @param low        a node representing the lower array bound
 * @param high       a node representing the upper array bound
 */
ir_node *new_d_Bounds (dbg_info *db, ir_node *store, 
		       ir_node *index, ir_node *low, ir_node *high);

#define  new_r_Bounds(g, b, s, i, l, h) new_rd_Bounds(NULL, g, b, s, i, l, h)
#define  new_Bounds(s, i, l, h)         new_d_Bounds (NULL,       s, i, l, h)

/** Returns non-zero if a given node is a Bound pseudo operation. */
int      is_Bounds(ir_node *bounds);

/** Returns the memory input of a Bound operation. */
ir_node *get_Bounds_mem(ir_node *bounds);

/** Returns the index input of a Bound operation. */
ir_node *get_Bounds_index(ir_node *bounds);

/** Returns the low bound input of a Bound operation. */
ir_node *get_Bounds_low(ir_node *bounds);

/** Returns the high bound input of a Bound operation. */
ir_node *get_Bounds_high(ir_node *bounds);

/** Sets the ir_graph of the pseudo operation Bounds. */
void      set_Bounds_semantics(ir_graph *g);

/** Return the ir_graph of the pseudo operation Bounds. */
ir_graph *get_Bounds_semantics(void);

#ifdef __cplusplus
}
#endif

#endif /* __PSEUDO_OP_H */
