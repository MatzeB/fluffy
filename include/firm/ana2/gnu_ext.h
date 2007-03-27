/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/gnu_ext.h
   Purpose:     Provide some GNU CC extensions to the rest of the world
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2005 Universit�t Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

/*
  gnu_ext: Provide some GNU CC extensions to the rest of the world
*/

/* Includes */

/* Local Defines: */
# if !defined (__GNUC__)
#  if !defined(__FUNCTION__)
#    define __FUNCTION__  "::"
#  endif
#  if !defined(__PRETTY_FUNCTION__)
#    define __PRETTY_FUNCTION__ ":::"
#  endif
# endif /* !define __GNUC__ */

/* Local Data Types: */

/* Local Variables: */

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */

/* ===================================================
   Exported Implementation:
   =================================================== */


/*
  $Log: gnu_ext.h,v $
  Revision 1.2  2006/07/02 16:30:17  beck
  Fixed warnings on newer VC

  Revision 1.1  2005/01/14 14:15:19  liekweg
  Support GNU extensions on non-GNU platforms


*/
