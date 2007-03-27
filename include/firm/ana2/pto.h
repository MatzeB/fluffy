/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto.h
   Purpose:     Import all includes needed for PTO/Entry to PTO
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universit�t Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/


# ifndef _PTO_
# define _PTO_

# include "pto_comp.h"

/* ===================================================
   Global Defines:
   =================================================== */
# define N_INITIAL_OJBS     10

/* ===================================================
 Global Data Types:
 =================================================== */

/* ===================================================
   Global Prototypes:
   =================================================== */
/* Perform PTO on all visible graphs. */
void pto_init (int);
void pto_run (void);
/* Dump all interesting stuff to a bunch of files */
void pto_dump (void);
/* Clean up our mess */
void pto_cleanup (void);

/* ===================================================
   Global Variables:
   =================================================== */


# endif /* not defined _PTO_ */



/*
  $Log: pto.h,v $
  Revision 1.7  2004/11/30 15:49:27  liekweg
  include 'dump'

  Revision 1.6  2004/11/24 14:53:55  liekweg
  Bugfixes

  Revision 1.5  2004/11/18 16:37:07  liekweg
  rewrite


*/
