/*
 * Project:     libFIRM
 * File name:   ir/ir/irtools.h
 * Purpose:     Some often needed tool-functions
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2006 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRM_COMMON_IRTOOLS_H_
#define _FIRM_COMMON_IRTOOLS_H_

#include "firm_config.h"
#include "firm_types.h"

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
lc_opt_entry_t *firm_opt_get_root(void);
#endif

#include "pset.h"

#undef MIN
#undef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

/**
 * Three valued compare as demanded by e.g. qsort(3)
 * @param c A number.
 * @param d Another number.
 * @return 0 if c == d, -1 if c < d, 1 if c > d.
 */
#define QSORT_CMP(c, d) (((c) > (d)) - ((c) < (d)))


/**
 * convert an integer into pointer
 */
#define INT_TO_PTR(v)   ((void *)((char *)0 + (v)))

/**
 * convert a pointer into an integer
 */
#define PTR_TO_INT(v)   ((int)((char *)(v) - (char *)0))

/** 
 * Dump a pset containing Firm objects.
 */
void firm_pset_dump(pset *set);

/**
 * The famous clear_link() walker-function.
 * Do not implement it by yourself, use this one
 */
void firm_clear_link(ir_node *n, void *env);

/**
 * Copies a node to a new irg. The Ins of the new node point to
 * the predecessors on the old irg.  n->link points to the new node.
 *
 * @param n    The node to be copied
 * @param irg  the new irg
 *
 * Does NOT copy standard nodes like Start, End etc that are fixed
 * in an irg. Instead, the corresponding nodes of the new irg are returned.
 * Note further, that the new nodes have no block.
 */
void copy_irn_to_irg(ir_node *n, ir_graph *irg);

/**
 * Creates an exact copy of a node.
 * The copy resists on the same graph in the same block.
 *
 * @param n   the node to copy
 *
 * @note If the copy is not changed, the next CSE operation will
 *       replace it by the original, so beware.
 */
ir_node *exact_copy(const ir_node *n);

#endif /* _FIRM_COMMON_IRTOOLS_H_ */

