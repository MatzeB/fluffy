/**
 * @file   iredgekinds.h 
 * @date   29.08.2006
 * @author Sebastian Hack
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _IREDGEKINDS_H
#define _IREDGEKINDS_H

/** Supported Edge kinds. */
enum _ir_edge_kind_t {
	EDGE_KIND_NORMAL,  /**< Normal data flow edges. */
	EDGE_KIND_BLOCK,   /**< Block to Block control flow edges. */
	EDGE_KIND_DEP,     /**< Dependency edges. */
	EDGE_KIND_LAST
};

typedef enum _ir_edge_kind_t ir_edge_kind_t;

/*
 * It's ugly but we need this forward ref. 
 */

void edges_notify_edge_kind(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt, ir_edge_kind_t kind, ir_graph *irg);

#endif /* _IREDGEKINDS_H */
