/*
 * Project:     firmlower
 * File name:   src/firmlower.h
 * Purpose:     Central header. 
 * Author:      Boris Boesler
 * Modified by:
 * Created:     10.07.2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001 Universität Karlsruhe. All rights reserved.
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef __FIRMLOWER_H__
#define __FIRMLOWER_H__

#include <libfirm/firm.h>
#include "lower_exceptions.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Transforms classes containing methods to classes with explicit
 * dispatch tables.
 *
 * Generates the following types/entities:
 * - An entity "dispatch table pointer" that is a constant field in each
 *   class.  It's a type_pointer where points_to is a "dispatch table type".
 * - A type that describes a dispatch table (dispatch table type).  It contains
 *   all the methods.
 * - A type that describes a dispatch table pointer.
 * - A constant entity "dispatch table" that is owned by global_type.  This is
 *   the dispatch table that must be dumped to the data segment.  The address of
 *   this table is the constant value of "dispatch table pointer"
 * - The constant value for the dispatch table pointer is added to constant 
 *   classes (entities of global type that are classes).
 *
 * Further it transforms the code:
 * - Introduces the additional dereferencing when accessing method pointers.
 *
 * Arranges the information in the following way:
 * class->link contains the dispatch type. (which is also a class type)
 * dispatch_type->link contains the dispatch table entity.
 * class->member[0] is the dispatch pointer field.
 * dispatch_table_entity->link contains the class type.
 *
 * @param DispTabPtr_Prefix    Suffix for entity in class
 * @param DispTabT_Prefix      Prefix for disp table type
 * @param DispTabPtrT_Prefix   Suffix for ptr type
 * @param DispTab_Prefix       Suffix for disp tab entity
 */
void introduce_method_dispatch (ident *dtpei, ident *dtti, ident *pti, ident *dtei) ;
ident *get_dipatch_table_description_prefix(void);
/**
 * Moves static allocated member entities of dynamic classes to global type.
 * Adapts access by Sel nodes.  Does not move methods. 
 */
void isolate_static_entities(void);

/* Fixes the object layout and the layout of the dispatch tables.  
   Preconditions:
   * All members must have fixed layout (no 'value' classes!!) 
   * It must be possible that all overwritten fields get the same offset.
   * 
   Inputs:
   * the prefix for classes that are dispatch tables
   * the alignment in bytes for types that are not representable as modes
     (get_type_mode returns NULL).
     The size of such types will be rounded up to a size dividable by this 
     alignment. 
   Actions:
   * Resolves inheritance by representing inherited fields/methods
     explicitly.
   * Compute Offset of fields. Dispatch table pointer gets offset 0.
     Assumes that dispatch table pointer is at position 0.  If a
     class represents a dispatch table type a method (or NULL) must
     be at position 0.
     Compacts the class layout: First copies the offsets for inherited
     fields.  Then fills gaps in the layout with new fields.  Then adds
     the remaining new fields at the end, sorted descending by size. 
     Keeps to the size and alignment given in modes and types. 
   * Computes size of class types.  */
void fix_class_layout(int default_alignment);

/**
 * Sets layout_fixed in array types. 
 */
void fix_array_layout(void);

/** 
 * Sets layout_fixed in stack frame types. 
 * These may not have any fields.
 */
void fix_stack_frame_layout(void);


/* Transforms all Firm graphs so that they are in phase "phase_low". The layout
   of all types referenced in the program must be fixed. 
   * Replaces SymConst Size by a real constant.
   * Replace Sel nodes by address computation.  Also resolves array access.
     @@@ so far only one dimensional arrays implemented
   * generates explicit initialization of memory allocated by Alloc nodes. */

/**
 * Removes all class types by replacing them by struct types.  Names of
 * structs are "ObjectDescriptionPrefix"<classname>. Replaces method entities
 * by pointer to method entities and moves the method entities to global type.
 * Allocates method pointer type with name <method type name>_"MethodPointerTypeSuffix".
 * Method pointer entity gets name "MethodPointerEntPrefix"
 * <method entity>.  Adapts Sel nodes to this change.
 *
 * Further adapts constant entities.  Changes link field of all entities.  Also
 * transforms frame types.  (Does not change the global type.) 
 * 
 * @param MethodPointerTypeSuffix   Suffix of pointer type pointing to method.
 * @param MethodPointerEntPrefix    Prefix of entity containing method pointer. 
 */
void lower_classes_to_struct(
  const char *MethodPointerTypeSuffix, 
  const char *MethodPointerEntPrefix   
);

/**
 * Mangle the ld ident of a static entity.
 * new ident is classname '_' entityname
 */
void set_static_entity_ld_ident(ir_entity *ent);

/**
 * describes, how to treat the memory block returned by an alloc node
 */
typedef enum {
  alloc_init_bad,
  alloc_init_zero,  /**< assumes that the alloc command generated by the backend
		       returns memory containing zeroes.  Stores initializing 
		       memory to zero are omitted. */
  alloc_noinit      /**< Generates stores initializing fields to zero. */
} alloc_sem_kinds;

/**
 * Replaces SymConst Size by a real constant.
 * Replace Sel nodes by address computation.  Also resolves array access. 
 * Adds explicit initialization code after Alloc nodes.
 *
 * @param k   sets the semantics of allocated memory
 */
void lower_firm(alloc_sem_kinds k);

#ifdef __cplusplus
}
#endif

#endif /* __FIRMLOWER_H__ */

