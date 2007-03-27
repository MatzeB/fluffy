/* -*- c -*- */

/*
  Project:     libFIRM
  File name:   ir/ana/pto_name.h
  Purpose:     Names for abstract objects
  Author:      Florian
  Modified by:
  Created:     Sat Nov 13 19:35:27 CET 2004
  CVS-ID:      $Id$
  Copyright:   (c) 1999-2004 Universit�t Karlsruhe
  Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/


# ifndef _PTO_NAME_
# define _PTO_NAME_

# include "pto_comp.h"          /* for pto_t */
# include "irnode.h"
# include "type.h"
# include "qset.h"

/* ===================================================
   Global Defines:
   =================================================== */

/* ===================================================
   Global Data Types:
   =================================================== */
typedef enum desc_kind_enum {
  none,
  object,
  array
} desc_kind_t;

/* abstract super class for all descriptors */
typedef struct desc_str
{
  int id;
  int visit;
  int ctx;
  int col_idx;
  desc_kind_t kind;
  ir_type *tp;
  ir_node *node;                /* allocation node */
  struct desc_str *prev;        /* linked list */
} desc_t;

/* object descriptor */
typedef struct obj_desc_str
{
  int id;
  int visit;
  int ctx;
  int col_idx;
  desc_kind_t kind;
  ir_type *tp;
  ir_node *node;                /* allocation node */
  struct desc_str *prev;        /* linked list */

  int n_fields;
  ir_entity **fields;
  qset_t **values;
} obj_desc_t;

/* array descriptor */
typedef struct arr_desc_str
{
  int id;
  int visit;
  int ctx;
  int col_idx;
  desc_kind_t kind;
  ir_type *tp;
  ir_node *node;                /* allocation node */
  struct desc_str *prev;        /* linked list */

  qset_t *value;
} arr_desc_t;

/* ===================================================
   Global Prototypes:
   =================================================== */
/* Dump all names to a file of the given name */
void pto_dump_names (const char*);

/* Find the given descriptor's entry for the given entity */
qset_t *get_entry (desc_t*, ir_entity*);

/* get a new descriptor for the given type at the given node */
desc_t *new_name (ir_type*, ir_node*, int);

/* get a new descriptor for the given (presumably static) entity */
desc_t *new_ent_name (ir_entity*);

/* Initialise the name module */
void pto_name_init (void);

/* Cleanup the name module */
void pto_name_cleanup (void);

/* ===================================================
   Global Variables:
   =================================================== */


# endif /* not defined _PTO_NAME_ */



/*
  $Log: pto_name.h,v $
  Revision 1.8  2006/12/13 19:46:47  beck
  rename type entity into ir_entity

  Revision 1.7  2006/01/13 22:00:15  beck
  renamed all types 'type' to 'ir_type'

  Revision 1.6  2004/12/15 13:30:41  liekweg
  print yet nicer names

  Revision 1.5  2004/12/06 12:52:09  liekweg
  colorize name dump

  Revision 1.4  2004/11/30 15:49:27  liekweg
  include 'dump'

  Revision 1.3  2004/11/30 14:47:54  liekweg
  fix initialisation; do correct iteration

  Revision 1.2  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
