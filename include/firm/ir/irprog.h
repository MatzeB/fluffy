/*
 * Project:     libFIRM
 * File name:   ir/ir/irprog.h
 * Purpose:     Entry point to the representation of a whole program.
 * Author:      Goetz Lindenmaier 
 * Modified by: 
 * Created:     2000
 * CVS-ID:      $Id$
 * Copyright:   (c) 2000-2003 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irprog.h
 *
 * ir representation of a program.
 * 
 * @author Goetz Lindenmaier
 *  
 * This file defines a construct that keeps all information about a
 * program:
 *   - A reference point to the method to be executed on program start. 
 *   - A list of all procedures.
 *   - A list of all types.
 *   - A global type that contais all global variables and procedures that do 
 *     not belong to a class.  This type represents the data segment of the 
 *     program.  It is not the base class of
 *     all classes in a class hierarchy (as, e.g., "object" in java).
 *   - A degenerated graph that contains constant expressions.
 *   - interprocedural outs state. 
 *   - a flag indicating validity of the interprocedural representation.
 *   - the output file name
 */

#ifndef _FIRM_IR_IRPROG_H_
#define _FIRM_IR_IRPROG_H_

#include "firm_types.h"
#include "irgraph.h"


/**
 * Datastructure that holds central information about a program
 * 
 * Preliminary documentation ;-)
 *
 * - main_irg:  The ir graph that is the entry point to the program.
 *              (Anything not reachable from here may be optimized away.
 *              If we want to translate libraries or the like correctly
 *              we must replace this by a list.)
 * - irg:       List of all ir graphs in the program.
 * - type:      A list containing all types known to the translated program.
 *              Some types can have several entries in this list (as a result of
 *              using exchange_types()).
 * - glob_type: The unique global type that is owner of all global entities.
 * 
 */
typedef struct ir_prog ir_prog;

/**
 * A variable from where everything in the ir can be accessed.
 * This variable contains the irp, the "immediate representation program".
 * This variable should be considered constant. Moreover, one should use get_irp()
 * to get access the the irp.
 *
 * @note
 * 	Think of the irp as the "handle" of libFirm.
 */
extern ir_prog *irp;

/**
 * Returns the access points from where everything in the ir can be accessed. 
 *
 * @see irp 
 */
ir_prog *get_irp(void);

/** Creates a new ir_prog, returns it and sets irp with it.
 *  Automatically called by init_firm() through init_irprog. */
ir_prog *new_ir_prog (void);

/** frees all memory used by irp.  Types in type list and irgs in irg
 *  list must be freed by hand before. */
void     free_ir_prog(void);

/** Sets the file name / executable name or the like. Initially the 
    ident 'no_name_set'. */
void   set_irp_prog_name (ident *name);

/** Returns true if the user ever set a program name */
int    irp_prog_name_is_set(void);

/** Gets the file name / executable name or the like. 
 */
ident *get_irp_prog_ident(void);

/** Gets the file name / executable name or the like. 
 */
const char *get_irp_prog_name (void);

/** Gets the main routine of the compiled program. */
ir_graph *get_irp_main_irg(void);

/** Sets the main routine of the compiled program. */
void      set_irp_main_irg(ir_graph *main_irg);

/** Adds irg to the list of ir graphs in irp. */
void      add_irp_irg(ir_graph *irg);

/** Removes irg from the list of irgs and
    shrinks the list by one. */
void      remove_irp_irg_from_list(ir_graph *irg);
/** Removes irg from the list of irgs, deallocates it and
    shrinks the list by one. */
void      remove_irp_irg(ir_graph *irg);

/** Returns the number of ir graphs in the irp. */
int       get_irp_n_irgs(void);

/** Returns the ir graph at position pos in the irp. */
ir_graph *get_irp_irg(int pos);

/** Sets the ir graph at position pos. */
void      set_irp_irg(int pos, ir_graph *irg);

/** Gets the number of graphs _and_ pseudo graphs. */
int       get_irp_n_allirgs(void);

/** Returns the ir graph at position pos of all graphs (including
 pseudo graphs).  Visits first graphs, then pseudo graphs. */
ir_graph *get_irp_allirg(int pos);

/** 
 * Returns the "global" type of the irp. 
 * Upon creation this is an empty class type.
 */
ir_type *get_glob_type(void);

/** 
 * Returns the "thread local storage" type of the irp.
 * Upon creation this is an empty struct type.
 */
ir_type *get_tls_type(void);

/** Adds type to the list of types in irp. */
void  add_irp_type(ir_type *typ);

/** Removes type from the list of types, deallocates it and
    shrinks the list by one. */
void  remove_irp_type(ir_type *typ);

/** Returns the number of all types in the irp. */
int   get_irp_n_types(void);

/** Returns the type at position pos in the irp. */
ir_type *get_irp_type(int pos);

/** Overwrites the type at position pos with another type. */
void  set_irp_type(int pos, ir_type *typ);

/** Returns the number of all modes in the irp. */
int   get_irp_n_modes(void);

/** Returns the mode at position pos in the irp. */
ir_mode *get_irp_mode(int pos);

/** Adds opcode to the list of opcodes in irp. */
void  add_irp_opcode(ir_op *opcode);

/** Removes opcode from the list of opcodes, deallocates it and
    shrinks the list by one. */
void  remove_irp_opcode(ir_op *opcode);

/** Returns the number of all opcodes in the irp. */
int   get_irp_n_opcodes(void);

/** Returns the opcode at position pos in the irp. */
ir_op *get_irp_opcode(int pos);

/** Sets the generic function pointer of all opcodes to NULL */
void  clear_irp_opcodes_generic_func(void);


/**  Return the graph for global constants. 
 *
 *   Returns an irgraph that only contains constant expressions for
 *   constant entities.  Do not use any access function for this
 *   graph, do not generate code for this graph.  This graph contains
 *   only one block.  The constant expressions may not contain control
 *   flow.   
 *   Walking the graph starting from any node will not reach the block 
 *   or any controlflow.
 *   See also copy_const_code() in entity.h.
 */
ir_graph *get_const_code_irg(void);


/** The phase state for the program.
 *
 *  The phase state of the whole program is
 *   building:  if at least one graph is state_building
 *              or one type is incomplete.
 *   high:      all graphs are in state high or low, all types are constructed.
 *   low:       all graphs are in state low, all types are in state layout fixed. 
 */
irg_phase_state get_irp_phase_state(void);
void            set_irp_phase_state(irg_phase_state s);

irg_outs_state get_irp_ip_outs_state(void);
void           set_irp_ip_outs_inconsistent(void);


irg_callee_info_state get_irp_callee_info_state(void);
void                  set_irp_callee_info_state(irg_callee_info_state s);

#endif /* ifndef _FIRM_IR_IRPROG_H_ */
