/*
* Project:     firmlower
* File name:   src/lower_exceptions.h
* Purpose:     lower exception nodes according to varying semantics. 
* Author:      Goetz Lindenmaier
* Modified by:
* Created:     17.9.2003
* CVS-ID:      $Id$
* Copyright:   (c) 2003 Universität Karlsruhe. All rights reserved.
* Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
*/

#ifndef __LOWER_EXCEPTIONS_H__
#define __LOWER_EXCEPTIONS_H__

#include <libfirm/firm.h>

/* default names */
#define GENEXC_TYPE_NAME "GenExc_Type"
#define GENEXC_RES_TYPE_NAME "GenExc_ResType"
#define GENEXC_METH_NAME_PREFIX "__GenExc"
#define INT_TYPE_NAME "__Int"
#define UINT_TYPE_NAME "__UInt"
#define EXC_EXIT_VAR_NAME "exc_set"
#define EXC_STORE_NAME "exc"

#define EXCEPTION_EXIT_VAL EXIT_FAILURE
#define EXIT_CALL_NAME "exit"
#define EXIT_CALL_TYPE_NAME "__exit"

/**  Removes exception testing and exception branches as far as possible.
 *
 *  Problem:  We can not remove explicit Raise nodes that branch to 
 *  End node -- this would result either in dangling code, or, replacing
 *  the Raise by a Jmp, in illegal Firm. We can only do guesses in such cases.
 *  Such Raise nodes are not removed.
 *
 *  So we remove only the X edges from fragile operations as Div...
 *
 */

/** Creates explicit code for implicit exceptions (Div by 0,...)
 * @param exit_on_unhandled If this parameter evaluates to true, do only local exception handling
 *                          and exit with no-zero exit code, if this is not possible.
 **/
enum lower_exception_flags {
  lower_exceptions_explicit_f          = 1,
  lower_exceptions_exit_on_unhandled_f = 2
};

void lower_exceptions(int flags);

/** Replace Bounds operations by explicit code. 
 *
 *  Derived from lower_exceptions. Inlines the called Bounds methods. */
void lower_Bounds_operation(void);

/**
 * test against capabilities of the C_GENERIC codegen
 * ie. all exceptions are transformed into (conditional) jumps
 **/
int is_exception_code_lowered( void );

/**
 * Register a function to call if removing implicit exception.
 *
 * @param alloc_ent entity of the called method. 
 * @param op        Indicates for which exception operation the function 
 *                  shall be called.  
 *
 * The called function must allocate a new exception object and store
 * this so that the handler can deal with it.  Further the method returns
 * a pointer that can be passed to Raise nodes.  After the call we either 
 * directly branch to the exception handler with a Jmp, or, if no handler
 * available, we branch to End with a Raise that consumes the result of 
 * the Call. 
 * The signature of the Call is 
 *   void *<fctname> (void).  
 *
 * Per default an exception constructor with name 
 * GENEXC_METH_NAME_PREFIX_<opcode> is called. 
 *
 * @@@ The method should get arguments representing line number and 
 * other debug information needed to construct the exception object. 
 *
 */
void register_exception_allocator(ir_entity *alloc_ent, const ir_op *exc_op);

/**
 * Register the type of the exception store and create it.
 */
void register_exception_ptr_type(ir_type *exc_ptr);

/**
 * Return the exception store entity.
 */
ir_entity *get_exception_store(void);

#endif /* __LOWER_EXCEPTIONS_H__ */
