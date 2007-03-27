/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/external/pseudo_irg.h
 * Purpose:     interface to pseudo irgs
 * Author:      G"otz Lindenmaier
 * Modified by: Boris Boesler
 * Created:     xx.10.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universit�t Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _FIRM_IR_PSEUDO_IRG_H_
#define _FIRM_IR_PSEUDO_IRG_H_

#include "firm_types.h"

/** Create a new ir graph to build a pseudo representation of a procedure.
 * 
 *  The pseudo representation can only be used for analyses.  It may not be
 *  optimized.  Pseudo graphs are kept in a separate graph list in irprog. 
 */
ir_graph *new_pseudo_ir_graph(ir_entity *ent, int n_loc);

/** Returns non-zero ir ir_graph is pseudo graph.
 *  Is irg a pseudo graph for analysis? */
int      is_pseudo_ir_graph(ir_graph *irg); 

/** Returns the number of pseudo graphs in the program. */
int get_irp_n_pseudo_irgs(void);

/** Returns the pos'th  pseudo graph in the program. */
ir_graph *get_irp_pseudo_irg(int pos);


/** If set, get_irp_n_irgs() and get_irp_irg() behave as if all pseudo
    graphs are in the irg list. If not set, get_entity_irg() returns
    NULL if the entity refers to a pseudo irg. */
void set_visit_pseudo_irgs(int x);
int  get_visit_pseudo_irgs(void);

#endif /* _FIRM_IR_PSEUDO_IRG_H_ */
