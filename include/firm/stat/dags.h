/*
 * Project:     libFIRM
 * File name:   ir/ir/dags.h
 * Purpose:     Statistics for Firm. DAG's in graphs.
 * Author:      Michael Beck 
 * Created:     
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _DAGS_H_
#define _DAGS_H_

#include "firmstat_t.h"
/*
 * count the DAG's size of a graph
 */
void count_dags_in_graph(graph_entry_t *global, graph_entry_t *graph);

#endif /* _DAGS_H_ */

