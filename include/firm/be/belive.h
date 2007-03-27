/**
 * Interblock liveness analysis.
 * @author Sebastian Hack
 * @date   6.12.2004
 * @cvs-id $Id$
 */

#ifndef _BELIVE_H
#define _BELIVE_H

#include "firm_types.h"
#include "pset.h"
#include "bearch_t.h"

#include <stdio.h>

typedef enum {
	be_lv_state_in  = 1,
	be_lv_state_end = 2,
	be_lv_state_out = 4,
} be_lv_state_t;

typedef struct _be_lv_t be_lv_t;

typedef struct _be_lv_info_t be_lv_info_t;


/**
 * Compute the inter block liveness for a graph.
 * @param irg The graph.
 */
be_lv_t *be_liveness(ir_graph *irg);

/**
 * Check the given liveness information against a freshly computed one.
 */
void be_liveness_check(be_lv_t *lv);

/**
 * Free the liveness information.
 */
void be_liveness_free(be_lv_t *lv);

/**
 * Recompute the complete liveness information.
 */
void be_liveness_recompute(be_lv_t *lv);

/**
 * Update the liveness information for a single node. 
 * It is irrelevant if there is liveness information present for the node. 
 * The liveness information for the node is firstly deleted and then recompute.
 * So, if the node is fresh and never recorded inf the liveness information
 * before, it is more efficient to call be_liveness_introduce().
 */
void be_liveness_update(be_lv_t *lv, ir_node *irn);

/**
 * Remove a node from the liveness information. 
 */
void be_liveness_remove(be_lv_t *lv, ir_node *irn);

/**
 * Introduce a new node to the liveness information. 
 * The new irn is not deleted from any block's liveness information, so it must be fresh!
 * @param lv The liveness info.
 * @param irn The node.
 */
void be_liveness_introduce(be_lv_t *lv, ir_node *irn);

/**
 * Add all nodes which are missing in the current liveness data.
 * The liveness data of the already existing nodes (in the liveness data) is not touched. 
 * @param The liveness info.
 */
void be_liveness_add_missing(be_lv_t *lv);

/**
 * Dump the liveness information for a graph.
 * @param f The output.
 * @param irg The graph.
 */
void be_liveness_dump(const be_lv_t *lv, FILE *f);

/**
 * Dump the liveness information for a graph.
 * @param irg The graph.
 * @param cls_name A string used as substring in the filename.
 */
void be_liveness_dumpto(const be_lv_t *lv, const char *cls_name);

/**
 * Check, if a node is live in at a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the entrance of @p block, 0 if not.
 */
int (be_is_live_in)(const be_lv_t *lv, const ir_node *block, const ir_node *irn);

/**
 * Check, if a node is live out at a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the exit of @p block, 0 if not.
 */
int (be_is_live_out)(const be_lv_t *lv, const ir_node *block, const ir_node *irn);

/**
 * Check, if a node is live at the end of a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the end of the block, 0 if not.
 */
int (be_is_live_end)(const be_lv_t *lv, const ir_node *block, const ir_node *irn);

/**
 * Check, if the SSA dominance property is fulfilled.
 * @param irg The graph.
 * @return   1 if dominance property is fulfilled, 0 otherwise
 */
int be_check_dominance(ir_graph *irg);

/**
 * The liveness transfer function.
 * Updates a live set over a single step from a given node to its predecessor.
 * Everything defined at the node is removed from the set, the uses of the node get inserted.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param irn      The node at which liveness should be computed.
 * @param live     The set of nodes live before @p irn. This set gets modified by updating it to
 *                 the nodes live after irn.
 * @return live.
 */
pset *be_liveness_transfer(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_node *irn, pset *live);

/**
 * Put all node live at the end of a block into a set.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param bl       The block.
 * @param live     The set to put them into.
 * @return live.
 */
pset *be_liveness_end_of_block(const be_lv_t *lv, const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *bl, pset *live);

/**
 * Compute a set of nodes which are live at another node.
 * BEWARE: This is the liveness immediately after the node,
 *         so the node itself is alive but it's operands maybe not.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param pos      The node.
 * @param live     The set to put them into.
 * @return live.
 */
pset *be_liveness_nodes_live_at(const be_lv_t *lv, const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *pos, pset *live);

/**
 * Compute a set of nodes which are live at another node.
 * BEWARE: This is the liveness immediately before the node,
 *         so the node itself is not alive but it's operands are.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param pos      The node.
 * @param live     The set to put them into.
 * @return live.
 */
pset *be_liveness_nodes_live_at_input(const be_lv_t *lv, const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *pos, pset *live);


/**
 * FIXME: Need comment
 */
void be_liveness_add_missing(be_lv_t *lv);

#endif /* _BELIVE_H */
