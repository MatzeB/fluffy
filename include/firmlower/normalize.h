/*
 * Project:     firmlower
 * File name:   src/normalize.c
 * Purpose:     functions that normalize the representation with respect to
 *              certain properties
 * Author:      Till Riedel
 * Modified by: 
 * Created:     4.6.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001, 2002 Universitaet Karlsruhe.  All rights reserved. 
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef __NORMALIZE_H__
#define __NORMALIZE_H__

#include "libfirm/firm.h"


/** Sets the mode of unary and binary operations.
 *
 *  Sets the mode of unary and binary operations.  
 *  

 * right NULL for unary operations
 */
void add_operation_mode(ir_op *op, ir_mode *left, ir_mode *right, ir_mode *res);

/** Default input and output mode. 
 * If default is not set normalize_modes_of_operations() asserts.  
 */
void add_default_operation_mode(ir_mode *m);

void normalize_modes_of_operations(void);

#endif /* __NORMALIZE_H__ */
