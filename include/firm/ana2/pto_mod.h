/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_mod.h
   Purpose:     Load/Store Transfer Functions
   Author:      Florian
   Modified by:
   Created:     Fri Nov 26 17:29:49 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universit�t Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/


#ifndef _PTO_MOD_
#define _PTO_MOD_

#include "irnode.h"
#include "entity.h"
#include "pto_comp.h"

/* ===================================================
   Global Defines:
   =================================================== */

/* ===================================================
 Global Data Types:
 =================================================== */

/* ===================================================
   Global Prototypes:
   =================================================== */
/* Perform the given store; return nonzero iff any involved values change */
int mod_store (ir_node*, ir_entity*, pto_t*, pto_t*);

/* Perform the given load; return nonzero iff any involved values change */
int mod_load  (ir_node*, ir_entity*, pto_t*);

/* ===================================================
   Global Variables:
   =================================================== */


#endif /* not defined _PTO_MOD_ */



/*
  $Log: pto_mod.h,v $
  Revision 1.2  2006/12/13 19:46:47  beck
  rename type entity into ir_entity

  Revision 1.1  2004/11/30 14:47:54  liekweg
  fix initialisation; do correct iteration


*/
