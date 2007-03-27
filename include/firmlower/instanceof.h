/**
 *
 * @file        instanceof.h
 *
 * Project:     firmlower                                                 <br>
 * File name:   src/instanceof.h                                          <br>
 * Purpose:     Introduce fields needed for instanceof in jack.           <br>
 * Author:      Florian Liekweg                                           <br>
 * Modified by:                                                           <br>
 * Created:     15.05.2002                                                <br>
 * Copyright:   (c) 2002-2003 IPD, Universität Karlsruhe                  <br>
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE. <br>
 * CVS-ID:      $Id$                                                      
 *
 */
#ifndef __INSTANCEOF_H__
#define __INSTANCEOF_H__

#include "firmlower.h"
#include <libfirm/firm_types.h>

#ifdef __cplusplus
extern "C" {
#endif

// =============================================
// Global Macros:
// =============================================

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

// =============================================
// Exported variables:
// =============================================

// -*- none -*-

// =============================================
// Exported data structures:
// =============================================

/*
  Bitmap data structure
*/

/* use char because sizeof (char) == 1 by ANSI and BYTES_PER_CHAR == 8 in C */
typedef struct bitmap_t 
{
  unsigned char *bytes;
  int n_bytes;
} bitmap_t;

/**
 * helper data structure for the 'introduce_instanceof_fields' pass
 */
typedef struct io_dat_t 
{
  bitmap_t *bitmap; /**< instanceof-bitmap */
  int dfn;          /**< dfn in class hierarchy */
  ir_type *clss;    /**< the class we're for */
} io_dat_t;

// =============================================
// Exported methods:
// =============================================

/**
 * Initialize dfn's and bitmaps.
 */
void      introduce_instanceof_fields (void);

/**
 * Get data for a class.
 *
 * @param clss  A class type.
 */
io_dat_t *find_io_dat (ir_type *clss);

/**
 * Get bitmap entity.
 *
 * @param clss  A class type.
 */
ir_entity *get_bitmap_entity       (ir_type *clss); 

/**
 * Return the instance_bitfield_type.
 */
ir_type *get_instance_bitfield_type (void); 

/**
 * Return the instance_bitfield_arr_type.
 */
ir_type *get_instance_bitfield_arr_type (void);

/**
 * Return the instance_bitfield_ptr_type.
 */
ir_type *get_instance_bitfield_ptr_type (void);

#ifdef __cplusplus
}
#endif

#endif /* defined __INSTANCEOF_H__ */
