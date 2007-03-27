/*
 * Project:     libFIRM                                                  
 * File name:   ir/tr/type.h                                             
 * Purpose:     Representation of types.                                 
 * Author:      Goetz Lindenmaier                                        
 * Modified by: Michael Beck                                             
 * Created:                                                              
 * Copyright:   (c) 2001-2006 Universit�t Karlsruhe                      
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-ID:      $Id$
 */

/**
 * @file type.h
 *
 *  Datastructure to hold type information.
 *
 *  This module supplies a datastructure to represent all types
 *  known in the compiled program.  This includes types specified
 *  in the program as well as types defined by the language.  In the
 *  view of the intermediate representation there is no difference
 *  between these types.  Finally it specifies some auxiliary types.
 *
 *  There exist several kinds of types, arranged by the structure of
 *  the type.  A type is described by a set of attributes.  Some of
 *  these attributes are common to all types, others depend on the
 *  kind of the type.
 *
 *  Types are different from the modes defined in irmode:  Types are
 *  on the level of the programming language, modes at the level of
 *  the target processor.
 *
 *  @see  tpop.h
 */
#ifndef _FIRM_TR_TYPE_H_
#define _FIRM_TR_TYPE_H_

#include "firm_types.h"
#include "tpop.h"
#include "firm_common.h"
#include "dbginfo.h"

/**
 *  An abstract data type to represent types.
 *
 *  This is the abstract data type with which any type known in the
 *  compiled program can be represented.  This includes types specified
 *  in the program as well as types defined by the language.  In the
 *  view of the intermediate representation there is no difference
 *  between these types.
 *
 *  There exist several kinds of types, arranged by the structure of
 *  the type.  These are distinguished by a type opcode.
 *  A type is described by a set of attributes.  Some of these attributes
 *  are common to all types, others depend on the kind of the type.
 *
 *  The following describes the common attributes.  They can only be
 *  accessed by the functions given below.
 *
 *  The common fields are:
 *
 *  - firm_kind:   A firm_kind tag containing k_type.  This is useful
 *                 for dynamically checking whether a node is a type node.
 *  - type_op:     A tp_op specifying the kind of the type.
 *  - name:        An identifier specifying the name of the type.  To be
 *                 set by the frontend.
 *  - visibility:  The visibility of this type.
 *  - size:        The size of the type, i.e. an entity of this type will
 *                 occupy size bits in memory.  In several cases this is
 *                 determined when fixing the layout of this type (class,
 *                 struct, union, array, enumeration).
 *  - alignment    The alignment of the type, i.e. an entity of this type will
 *                 be allocated an an address in memory with this alignment.
 *                 In several cases this is determined when fixing the layout
 *                 of this type (class, struct, union, array)
 *  - mode:        The mode to be used to represent the type on a machine.
 *  - state:       The state of the type.  The state represents whether the
 *                 layout of the type is undefined or fixed (values: layout_undefined
 *                 or layout_fixed).  Compound types can have an undefined
 *                 layout.  The layout of the basic types primitive and pointer
 *                 is always layout_fixed.  If the layout of
 *                 compound types is fixed all entities must have an offset
 *                 and the size of the type must be set.
 *                 A fixed layout for enumeration types means that each enumeration
 *                 is associated with an implementation value.
 *  - assoc_type:  The associated lowered/upper type.
 *  - visit:       A counter for walks of the type information.
 *  - link:        A void* to associate some additional information with the type.
 *
 *  These fields can only be accessed via access functions.
 *
 *  Depending on the value of @c type_op, i.e., depending on the kind of the
 *  type the adt contains further attributes.  These are documented below.
 *
 *  @see
 *
 *  @link class_type class @endlink, @link struct_type struct @endlink,
 *  @link method_type method @endlink, @link union_type union @endlink,
 *  @link array_type array @endlink, @link enumeration_type enumeration @endlink,
 *  @link pointer_type pointer @endlink, @link primitive_type primitive @endlink
 *
 *  @todo
 *      mode maybe not global field??
 */
#ifndef _IR_TYPE_TYPEDEF_
#define _IR_TYPE_TYPEDEF_
typedef struct ir_type ir_type;
#endif

# include "type_or_entity.h"

/** Frees all entities associated with a type.
 *  Does not free the array entity.
 *  Warning: ensure these entities are not referenced anywhere else.
 */
void        free_type_entities(ir_type *tp);

/** Frees the memory used by the type.
 *
 * Removes the type from the type list. Does not free the entities
 * belonging to the type, except for the array element entity.  Does
 * not free if tp is "none" or "unknown".  Frees entities in value
 * param subtypes of method types!!! Make sure these are not
 * referenced any more.  Further make sure there is no pointer type
 * that refers to this type.                           */
void        free_type(ir_type *tp);

const tp_op*get_type_tpop(const ir_type *tp);
ident*      get_type_tpop_nameid(const ir_type *tp);
const char* get_type_tpop_name(const ir_type *tp);
tp_opcode   get_type_tpop_code(const ir_type *tp);

ident*      get_type_ident(const ir_type *tp);
void        set_type_ident(ir_type *tp, ident* id);
const char* get_type_name(const ir_type *tp);

/** This enumeration flags the visibility of entities and types.  
 * 
 * This is necessary for partial compilation.  
 * We rely on the ordering of the flags. 
 */
typedef enum {
  visibility_local,              /**< The entity is only visible locally.  This is the default for 
                                      entities. 
                                      The type is only visible locally.  All instances are allocated 
                                      locally, and no pointer to entities of this type are passed 
                                      out of this compilation unit. */
  visibility_external_visible,   /**< The entity is visible to other external program parts, but
                                      it is defined here.  It may not be optimized away.  The entity must
                                      be static_allocated. 
                                      For types:  entities of this type can be accessed externally.  No
                                      instances of this type are allocated externally.  */
  visibility_external_allocated  /**< The entity is defined and allocated externally.  This compilation
                                      must not allocate memory for this entity. The entity must
                                      be static_allocated.  This can also be an external defined
                                      method. 
                                      For types:  entities of this type are allocated and accessed from
                                      external code.  Default for types.  */
} ir_visibility;

/** The visibility of a type. 
 *
 *  The visibility of a type indicates, whether entities of this type
 *  are accessed or allocated in external code.  
 *
 *  An entity of a type is allocated in external code, if the external
 *  code declares a variable of this type, or dynamically allocates
 *  an entity of this type.  If the external code declares a (compound)
 *  type, that contains entities of this type, the visibility also 
 *  must be external_allocated. 
 *  
 *  The visibility must be higher than that of all entities, if the 
 *  type is a compound.  Here it is questionable, what happens with
 *  static entities.  If these are accessed external by direct reference,
 *  (a static call to a method, that is also in the dispatch table) 
 *  it should not affect the visibility of the type.  
 *
 *
 * @@@ Do we need a visibility for types?  
 * I change the layout of types radically when doing type splitting.  
 * I need to know, which fields of classes are accessed in the RTS, 
 * e.g., [_length.  I may not move [_length to the split part.
 * The layout though, is a property of the type. 
 *
 * One could also think of changing the mode of a type ...
 * 
 * But, we could also output macros to access the fields, e.g.,
 *  ACCESS_[_length (X)   X->length              // conventional
 *  ACCESS_[_length (X)   X->_split_ref->length  // with type splitting
 * 
 * For now I implement this function, that returns the visibility 
 * based on the visibility of the entities of a compound ...
 *
 * This function returns visibility_external_visible if one or more
 * entities of a compound type have visibility_external_visible.
 * Entities of types are never visibility_external_allocated (right?).
 * Else returns visibility_local.
 */
ir_visibility get_type_visibility(const ir_type *tp);
void          set_type_visibility(ir_type *tp, ir_visibility v);



/** The state of the type layout. */
typedef enum {
  layout_undefined,    /**< The layout of this type is not defined.
                            Address computation to access fields is not
                            possible, fields must be accessed by Sel
                            nodes.  Enumeration constants might be undefined.
                            This is the default value except for
                            pointer, primitive and method types. */
  layout_fixed         /**< The layout is fixed, all component/member entities
                            have an offset assigned.  Size of the type is known.
                            Arrays can be accessed by explicit address
                            computation.  Enumeration constants must be defined.
                            Default for pointer, primitive and method types. */
} type_state;

/** Returns a human readable string for the enum entry. */
const char *get_type_state_name(type_state s);

/** Returns the type layout state of a type. */
type_state  get_type_state(const ir_type *tp);

/** Sets the type layout state of a type.
 *
 * For primitives, pointer and method types the layout is always fixed.
 * This call is legal but has no effect.
 */
void        set_type_state(ir_type *tp, type_state state);

/** Returns the mode of a type.
 *
 * Returns NULL for all non atomic types.
 */
ir_mode*    get_type_mode(const ir_type *tp);

/** Sets the mode of a type.
 *
 * Only has an effect on primitive, enumeration and pointer types.
 */
void        set_type_mode(ir_type *tp, ir_mode* m);

/** Returns the size of a type in bytes, returns -1 if the size is NOT
 *  a byte size, i.e. not dividable by 8. */
int         get_type_size_bytes(const ir_type *tp);

/** Returns the size of a type in bits. */
int         get_type_size_bits(const ir_type *tp);

/** Sets the size of a type in bytes.
 *
 * For primitive, enumeration, pointer and method types the size
 * is always fixed. This call is legal but has no effect.
 */
void        set_type_size_bytes(ir_type *tp, int size);

/** Sets the size of a type in bits.
 *
 * For primitive, enumeration, pointer and method types the size
 * is always fixed. This call is legal but has no effect.
 */
void        set_type_size_bits(ir_type *tp, int size);

/** Returns the alignment of a type in bytes.
 *
 *  Returns -1 if the alignment is NOT
 *  a byte size, i.e. not dividable by 8. Calls get_type_alignment_bits(). */
int         get_type_alignment_bytes(ir_type *tp);

/** Returns the alignment of a type in bits. 
 *
 *  If the alignment of a type is
 *  not set, it is calculated here according to the following rules:
 *  -#.) if a type has a mode, the alignment is the mode size.
 *  -#.) compound types have the alignment of there biggest member.
 *  -#.) array types have the alignment of there element type.
 *  -#.) method types return 0 here.
 *  -#.) all other types return 8 here (i.e. aligned at byte).
 */
int         get_type_alignment_bits(ir_type *tp);

/** Sets the alignment of a type in bytes. */
void        set_type_alignment_bytes(ir_type *tp, int size);

/** Sets the alignment of a type in bits.
 *
 * For method types the alignment is always fixed.
 * This call is legal but has no effect.
 */
void        set_type_alignment_bits(ir_type *tp, int size);

/** Returns the visited count of a type. */
unsigned long get_type_visited(const ir_type *tp);
/** Sets the visited count of a type to num. */
void          set_type_visited(ir_type *tp, unsigned long num);
/** Sets visited field in type to type_visited. */
void          mark_type_visited(ir_type *tp);
/** Returns non-zero if the type is already visited */
int           type_visited(const ir_type *tp);
/** Returns non-zero if the type is not yet visited */
int           type_not_visited(const ir_type *tp);

/** Returns the associated link field of a type. */
void*         get_type_link(const ir_type *tp);
/** Sets the associated link field of a type. */
void          set_type_link(ir_type *tp, void *l);

/**
 * Visited flag to traverse the type information.
 *
 * Increase this flag by one before traversing the type information
 * using inc_master_type_visited().
 * Mark type nodes as visited by mark_type_visited(ir_type).
 * Check whether node was already visited by type_visited(ir_type)
 * and type_not_visited(ir_type).
 * Or use the function to walk all types.
 *
 * @see  typewalk
 */
void          set_master_type_visited(unsigned long val);
unsigned long get_master_type_visited(void);
void          inc_master_type_visited(void);

/**
 * Checks whether a pointer points to a type.
 *
 * @param thing     an arbitrary pointer
 *
 * @return
 *     true if the thing is a type, else false
 */
int is_type(const void *thing);

/**
 *   Checks whether two types are structurally equal.
 *
 *   @param typ1  the first type
 *   @param typ2  the second type
 *
 *   @return
 *    true if the types are equal, else false.
 *
 *   Types are equal if :
 *    - they are the same type kind
 *    - they have the same name
 *    - they have the same mode (if applicable)
 *    - they have the same type_state and, ev., the same size
 *    - they are class types and have:
 *      - the same members (see same_entity in entity.h)
 *      - the same supertypes -- the C-pointers are compared --> no recursive call.
 *      - the same number of subtypes.  Subtypes are not compared,
 *        as this could cause a cyclic test.
 *      - the same peculiarity
 *    - they are structure types and have the same members
 *    - they are method types and have
 *      - the same parameter types
 *      - the same result types
 *    - they are union types and have the same members
 *    - they are array types and have
 *      - the same number of dimensions
 *      - the same dimension bounds
 *      - the same dimension order
 *      - the same element type
 *    - they are enumeration types and have the same enumerator names
 *    - they are pointer types and have the identical points_to type
 *      (i.e., the same C-struct to represent the type, type_id is skipped.
 *       This is to avoid endless recursions; with pointer types cyclic
 *       type graphs are possible.)
 */
int equal_type(ir_type *typ1, ir_type *typ2);

/**
 *   Checks whether two types are structural comparable.
 *
 *   @param st pointer type
 *   @param lt pointer type
 *
 *   @return
 *    true if type st is smaller than type lt, i.e. whenever
 *    lt is expected a st can be used.
 *    This is true if
 *    - they are the same type kind
 *    - mode(st) < mode (lt)  (if applicable)
 *    - they are class types and st is (transitive) subtype of lt,
 *    - they are structure types and
 *       - the members of st have exactly one counterpart in lt with the same name,
 *       - the counterpart has a bigger type.
 *    - they are method types and have
 *      - the same number of parameter and result types,
 *      - the parameter types of st are smaller than those of lt,
 *      - the result types of st are smaller than those of lt
 *    - they are union types and have the members of st have exactly one
 *      @return counterpart in lt and the type is smaller
 *    - they are array types and have
 *      - the same number of dimensions
 *      - all bounds of lt are bound of st
 *      - the same dimension order
 *      - the same element type
 *      @return or
 *      - the element type of st is smaller than that of lt
 *      - the element types have the same size and fixed layout.
 *    - they are enumeration types and have the same enumerator names
 *    - they are pointer types and have the points_to type of st is
 *      @return smaller than the points_to type of lt.
 *
 */
int smaller_type(ir_type *st, ir_type *lt);

/**
 *  @page class_type    Representation of a class type
 *
 *  If the type opcode is set to type_class the type represents class
 *  types.  A list of fields and methods is associated with a class.
 *  Further a class can inherit from and bequest to other classes.
 *
 *  The following attributes are private to this type kind:
 *  - member:     All entities belonging to this class.  This are method entities
 *                which have type_method or fields that can have any of the
 *                following type kinds: type_class, type_struct, type_union,
 *                type_array, type_enumeration, type_pointer, type_primitive.
 *
 *  The following two are dynamic lists that can be grown with an "add_" function,
 *  but not shrinked:
 *
 *  - subtypes:    A list of direct subclasses.
 *
 *  - supertypes:  A list of direct superclasses.
 *
 *  - peculiarity: The peculiarity of this class.  If the class is of peculiarity
 *                 "description" it only is a description of requirements to a class,
 *                 as, e.g., a Java interface.  The class will never be allocated.
 *                 Peculiarity inherited is only possible for entities.  An entity
 *                 is of peculiarity inherited if the compiler generated the entity
 *                 to explicitly resolve inheritance.  An inherited method entity has
 *                 no value for irg.
 *                 Values: description, existent, inherited.  Default: existent.
 *
 *  - type_info:   An entity representing the type information of this class.
 *                 This entity can be of arbitrari type, Firm did not use it yet.
 *                 It allows to express the coupling of a type with an entity
 *                 representing this type.  This information is useful for lowering
 *                 of InstOf and TypeChk nodes.  Default: NULL
 *
 *  - vtable_size: The size of this class virtual function table.
 *                 Default:  0
 *
 *  - final:       A final class is always a leaf in the class hierarchy.  Final
 *                 classes cannot be super classes of other ones.  As this information
 *                 can only be computed in whole world compilations, we allow to
 *                 set this flag.  It is used in optimizations if get_opt_closed_world()
 *                 is false.  Default:  false
 *
 *  - interface:   The class represents an interface.  This flag can be set to distinguish
 *                 between interfaces, abstract classes and other classes that all may
 *                 have the peculiarity peculiarity_description.  Depending on this flag
 *                 the lowering might do different actions.  Default:  false
 *
 *  - abstract :   The class represents an abstract class.  This flag can be set to distinguish
 *                 between interfaces, abstract classes and other classes that all may
 *                 have the peculiarity peculiarity_description.  Depending on this flag
 *                 the lowering might do different actions.  Default:  false
 */

/** Creates a new class type. */
ir_type *new_type_class (ident *name);

/** Creates a new class type with debug information. */
ir_type *new_d_type_class (ident *name, dbg_info *db);

/* --- manipulate private fields of class type  --- */

/** Adds the entity as member of the class.  */
void add_class_member   (ir_type *clss, ir_entity *member);

/** Returns the number of members of this class. */
int get_class_n_members (const ir_type *clss);

/** Returns the member at position pos, 0 <= pos < n_member */
ir_entity *get_class_member   (const ir_type *clss, int pos);

/** Returns index of mem in clss, -1 if not contained. */
int get_class_member_index(const ir_type *clss, ir_entity *mem);

/** Finds the member with name 'name'. If several members with the same
 *  name returns one of them.  Returns NULL if no member found. */
ir_entity *get_class_member_by_name(ir_type *clss, ident *name);

/** Overwrites the member at position pos, 0 <= pos < n_member with
 *  the passed entity. */
void set_class_member   (ir_type *clss, ir_entity *member, int pos);

/** Replaces complete member list in class type by the list passed.
 *
 *  Copies the list passed. This function is necessary to reduce the number of members.
 *  members is an array of entities, num the size of this array.  Sets all
 *  owners of the members passed to clss. */
void set_class_members  (ir_type *clss, ir_entity *members[], int arity);

/** Finds member in the list of members and removes it.
 *
 *  Shrinks the member list, so iterate from the end!!!
 *  Does not deallocate the entity.  */
void remove_class_member(ir_type *clss, ir_entity *member);


/** Adds subtype as subtype to clss.
 *
 *  Checks whether clss is a supertype of subtype.  If not
 *  adds also clss as supertype to subtype.  */
void    add_class_subtype   (ir_type *clss, ir_type *subtype);

/** Returns the number of subtypes */
int     get_class_n_subtypes (const ir_type *clss);

/** Gets the subtype at position pos, 0 <= pos < n_subtype. */
ir_type *get_class_subtype   (ir_type *clss, int pos);

/** Returns the index to access subclass as subtype of class.  
 *
 *  If subclass is no direct subtype of class returns -1.
 */
int get_class_subtype_index(ir_type *clss, const ir_type *subclass);

/** Sets the subtype at position pos, 0 <= pos < n_subtype.
 *
 *  Does not set the corresponding supertype relation for subtype: this might
 *  be a different position! */
void    set_class_subtype   (ir_type *clss, ir_type *subtype, int pos);

/** Finds subtype in the list of subtypes and removes it  */
void    remove_class_subtype(ir_type *clss, ir_type *subtype);

/* Convenience macros */
#define add_class_derived_type(clss, drvtype)       add_class_subtype(clss, drvtype)
#define get_class_n_derived_types(clss)             get_class_n_subtypes(clss)
#define get_class_derived_type(clss, pos)           get_class_subtype(clss, pos)
#define get_class_derived_type_index(clss, drvtype) get_class_subtype_index(clss, drvtype)
#define set_class_derived_type(clss, drvtype, pos)  set_class_subtype(clss, drvtype, pos)
#define remove_class_derived_type(clss, drvtype)    remove_class_subtype(clss, drvtype)

/** Adds supertype as supertype to class.
 *
 *  Checks whether clss is a subtype of supertype.  If not
 *  adds also clss as subtype to supertype.  */
void    add_class_supertype   (ir_type *clss, ir_type *supertype);

/** Returns the number of supertypes */
int     get_class_n_supertypes (const ir_type *clss);

/** Returns the index to access superclass as supertype of class.  
 *
 *  If superclass is no direct supertype of class returns -1.
 */
int     get_class_supertype_index(ir_type *clss, ir_type *super_clss);

/** Gets the supertype at position pos,  0 <= pos < n_supertype. */
ir_type *get_class_supertype   (ir_type *clss, int pos);

/** Sets the supertype at position pos, 0 <= pos < n_supertype.
 *
 *  Does not set the corresponding subtype relation for supertype: this might
 *  be at a different position! */
void    set_class_supertype   (ir_type *clss, ir_type *supertype, int pos);

/** Finds supertype in the list of supertypes and removes it */
void    remove_class_supertype(ir_type *clss, ir_type *supertype);

/** Convenience macro */
#define add_class_base_type(clss, basetype)  add_class_supertype(clss, basetype)
#define get_class_n_base_types(clss)  get_class_n_supertypes(clss)
#define get_class_base_type_index(clss, base_clss) get_class_supertype_index(clss, base_clss)
#define get_class_base_type(clss, pos)  get_class_supertype(clss, pos)
#define set_class_base_type(clss, basetype, pos) set_class_supertype(clss, basetype, pos)
#define remove_class_base_type(clss, basetype)  remove_class_supertype(clss, basetype)

/** Convenience macro */
#define add_class_base_type(clss, basetype)        add_class_supertype(clss, basetype)
#define get_class_n_base_types(clss)               get_class_n_supertypes(clss)
#define get_class_base_type_index(clss, base_clss) get_class_supertype_index(clss, base_clss)
#define get_class_base_type(clss, pos)             get_class_supertype(clss, pos)
#define set_class_base_type(clss, basetype, pos)   set_class_supertype(clss, basetype, pos)
#define remove_class_base_type(clss, basetype)     remove_class_supertype(clss, basetype)

/** This enumeration flags the peculiarity of entities and types. */
typedef enum {
  peculiarity_description,     /**< Represents only a description.  The entity/type is never
                            allocated, no code/data exists for this entity/type.
                        @@@ eventually rename to descriptive (adjective as the others!)*/
  peculiarity_inherited,       /**< Describes explicitly that other entities are
                            inherited to the owner of this entity.
                            Overwrites must refer to at least one other
                            entity.  If this is a method entity there exists
                            no irg for this entity, only for one of the
                            overwritten ones.
                        Only for entity. */
  peculiarity_existent         /**< The entity/type (can) exist.
                    @@@ eventually rename to 'real' i.e., 'echt'
                        This serves better as opposition to description _and_ inherited.*/
} ir_peculiarity;

/** Returns a human readable string for a peculiarity. */
const char *get_peculiarity_name(ir_peculiarity p);

/** Returns the peculiarity of the class. */
ir_peculiarity get_class_peculiarity (const ir_type *clss);
/** Sets the peculiarity of the class. */
void           set_class_peculiarity (ir_type *clss, ir_peculiarity pec);

/** Returns the type info entity of a class. */
ir_entity *get_class_type_info(const ir_type *clss);

/** Set a type info entity for the class. */
void set_class_type_info(ir_type *clss, ir_entity *ent);

/** Returns the size of the virtual function table. */
unsigned get_class_vtable_size(const ir_type *clss);

/** Sets a new size of the virtual function table. */
void set_class_vtable_size(ir_type *clss, unsigned size);

/** Returns non-zero if a class is final. */
int is_class_final(const ir_type *clss);

/** Sets the class final flag. */
void set_class_final(ir_type *clss, int flag);

/** Return non-zero if a class is an interface */
int is_class_interface(const ir_type *clss);

/** Sets the class interface flag. */
void set_class_interface(ir_type *clss, int flag);

/** Return non-zero if a class is an abstract class. */
int is_class_abstract(const ir_type *clss);

/** Sets the class abstract flag. */
void set_class_abstract(ir_type *clss, int flag);

/** Set and get a class' dfn --
   @todo This is an undocumented field, subject to change! */
void set_class_dfn (ir_type *clss, int dfn);
int  get_class_dfn (const ir_type *clss);

/** Returns true if a type is a class type. */
int is_Class_type(const ir_type *clss);

/**
 *  @page struct_type   Representation of a struct type
 *
 *  A struct type represents aggregate types that consist of a list
 *  of fields.
 *
 *  The following attributes are private to this type kind:
 *  - member:  All entities belonging to this class.  This are the fields
 *             that can have any of the following types:  type_class,
 *             type_struct, type_union, type_array, type_enumeration,
 *             type_pointer, type_primitive.
 *             This is a dynamic list that can be grown with an "add_" function,
 *             but not shrinked.
 *             This is a dynamic list that can be grown with an "add_" function,
 *             but not shrinked.
 */
/** Creates a new type struct */
ir_type *new_type_struct (ident *name);
/** Creates a new type struct with debug information. */
ir_type *new_d_type_struct (ident *name, dbg_info* db);

/* --- manipulate private fields of struct --- */

/** Adds the entity as member of the struct.  */
void add_struct_member   (ir_type *strct, ir_entity *member);

/** Returns the number of members of this struct. */
int get_struct_n_members (const ir_type *strct);

/** Returns the member at position pos, 0 <= pos < n_member */
ir_entity *get_struct_member   (const ir_type *strct, int pos);

/** Returns index of member in strct, -1 if not contained. */
int get_struct_member_index(const ir_type *strct, ir_entity *member);

/** Overwrites the member at position pos, 0 <= pos < n_member with
   the passed entity. */
void set_struct_member   (ir_type *strct, int pos, ir_entity *member);

/** Finds member in the list of members and removes it. */
void remove_struct_member (ir_type *strct, ir_entity *member);

/** Returns true if a type is a struct type. */
int is_Struct_type(const ir_type *strct);

/**
 * @page method_type    Representation of a method type
 *
 * A method type represents a method, function or procedure type.
 * It contains a list of the parameter and result types, as these
 * are part of the type description.  These lists should not
 * be changed by a optimization, as a change creates a new method
 * type.  Therefore optimizations should allocated new method types.
 * The set_ routines are only for construction by a frontend.
 *
 * - n_params:   Number of parameters to the procedure.
 *               A procedure in FIRM has only call by value parameters.
 *
 * - param_type: A list with the types of parameters.  This list is ordered.
 *               The nth type in this list corresponds to the nth element
 *               in the parameter tuple that is a result of the start node.
 *               (See ircons.h for more information.)
 *
 * - value_param_ents
 *               A list of entities (whose owner is a struct private to the
 *               method type) that represent parameters passed by value.
 *
 * - n_res:      The number of results of the method.  In general, procedures
 *               have zero results, functions one.
 *
 * - res_type:   A list with the types of parameters.  This list is ordered.
 *               The nth type in this list corresponds to the nth input to
 *               Return nodes.  (See ircons.h for more information.)
 *
 * - value_res_ents
 *               A list of entities (whose owner is a struct private to the
 *               method type) that represent results passed by value.
 */

/* These macros define the suffixes for the types and entities used
   to represent value parameters / results. */
#define VALUE_PARAMS_SUFFIX  "val_param"
#define VALUE_RESS_SUFFIX    "val_res"

/** Create a new method type.
 *
 * @param name      the name (ident) of this type
 * @param n_param   the number of parameters
 * @param n_res     the number of results
 *
 * The arrays for the parameter and result types are not initialized by
 * the constructor.
 */
ir_type *new_type_method (ident *name, int n_param, int n_res);

/** Create a new method type with debug information.
 *
 * @param name      the name (ident) of this type
 * @param n_param   the number of parameters
 * @param n_res     the number of results
 * @param db        user defined debug information
 *
 * The arrays for the parameter and result types are not initialized by
 * the constructor.
 */
ir_type *new_d_type_method (ident *name, int n_param, int n_res, dbg_info* db);

/* -- manipulate private fields of method. -- */

/** Returns the number of parameters of this method. */
int   get_method_n_params  (const ir_type *method);

/** Returns the type of the parameter at position pos of a method. */
ir_type *get_method_param_type(ir_type *method, int pos);
/** Sets the type of the parameter at position pos of a method.
    Also changes the type in the pass-by-value representation by just
    changing the type of the corresponding entity if the representation is constructed. */
void  set_method_param_type(ir_type *method, int pos, ir_type *tp);
/** Returns an entity that represents the copied value argument.  Only necessary
   for compounds passed by value. This information is constructed only on demand. */
ir_entity *get_method_value_param_ent(ir_type *method, int pos);
/**
 * Returns a type that represents the copied value arguments if one
 * was allocated, else NULL.
 */
ir_type *get_method_value_param_type(const ir_type *method);
/** Returns an ident representing the parameters name. Returns NULL if not set.
    For debug support only. */
ident *get_method_param_ident(ir_type *method, int pos);
/** Returns a string representing the parameters name. Returns NULL if not set.
    For debug support only. */
const char *get_method_param_name(ir_type *method, int pos);
/** Sets an ident representing the parameters name. For debug support only. */
void set_method_param_ident(ir_type *method, int pos, ident *id);

/** Returns the number of results of a method type. */
int   get_method_n_ress   (const ir_type *method);
/** Returns the return type of a method type at position pos. */
ir_type *get_method_res_type(ir_type *method, int pos);
/** Sets the type of the result at position pos of a method.
    Also changes the type in the pass-by-value representation by just
    changing the type of the corresponding entity if the representation is constructed. */
void  set_method_res_type(ir_type *method, int pos, ir_type *tp);
/** Returns an entity that represents the copied value result.  Only necessary
   for compounds passed by value. This information is constructed only on demand. */
ir_entity *get_method_value_res_ent(ir_type *method, int pos);

/**
 * Returns a type that represents the copied value results.
 */
ir_type *get_method_value_res_type(const ir_type *method);

/**
 * This enum flags the variadicity of methods (methods with a
 * variable amount of arguments (e.g. C's printf). Default is
 * non_variadic.
 */
typedef enum variadicity {
  variadicity_non_variadic, /**< non variadic */
  variadicity_variadic      /**< variadic */
} variadicity;

/** Returns the null-terminated name of this variadicity. */
const char *get_variadicity_name(variadicity vari);

/** Returns the variadicity of a method. */
variadicity get_method_variadicity(const ir_type *method);

/** Sets the variadicity of a method. */
void set_method_variadicity(ir_type *method, variadicity vari);

/**
 * Returns the first variadic parameter index of a type.
 * If this index was NOT set, the index of the last parameter
 * of the method type plus one is returned for variadic functions.
 * Non-variadic function types always return -1 here.
 */
int get_method_first_variadic_param_index(const ir_type *method);

/**
 * Sets the first variadic parameter index. This allows to specify
 * a complete call type (containing the type of all parameters)
 * but still have the knowledge, which parameter must be passed as
 * variadic one.
 */
void set_method_first_variadic_param_index(ir_type *method, int index);

/** 
 * Additional method type properties:
 * Tell about special properties of a method type. Some
 * of these may be discovered by analyses.
 */
typedef enum {
  mtp_no_property        = 0x00000000, /**< no additional properties, default */
  mtp_property_const     = 0x00000001, /**< This method did not access memory and calculates
                                         its return values solely from its parameters.
                                         GCC: __attribute__((const)). */
  mtp_property_pure      = 0x00000002, /**< This method did NOT write to memory and calculates
                                         its return values solely from its parameters and
                                         the memory they points to (or global vars).
                                         GCC: __attribute__((pure)). */
  mtp_property_noreturn  = 0x00000004, /**< This method did not return due to an aborting system
                                         call.
                                         GCC: __attribute__((noreturn)). */
  mtp_property_nothrow   = 0x00000008, /**< This method cannot throw an exception.
                                         GCC: __attribute__((nothrow)). */
  mtp_property_naked     = 0x00000010, /**< This method is naked.
                                         GCC: __attribute__((naked)). */
  mtp_property_malloc    = 0x00000020, /**< This method returns newly allocate memory.
                                         GCC: __attribute__((malloc)). */
  mtp_property_intrinsic = 0x00000040, /**< This method is intrinsic. It is expected that
                                         a lowering phase will remove all calls to it. */
  mtp_property_runtime   = 0x00000080, /**< This method represents a runtime routine. */
  mtp_property_inherited = (1<<31)     /**< Internal. Used only in irg's, means property is 
                                         inherited from type. */
} mtp_additional_property;

/** Returns the mask of the additional graph properties. */
unsigned get_method_additional_properties(const ir_type *method);

/** Sets the mask of the additional graph properties. */
void set_method_additional_properties(ir_type *method, unsigned property_mask);

/** Sets one additional graph property. */
void set_method_additional_property(ir_type *method, mtp_additional_property flag);

/**
 * Calling conventions: lower 24 bits are the number of register parameters,
 * upper 8 encode the calling conventions.
 */
typedef enum {
  cc_reg_param        = 0x01000000, /**< Transmit parameters in registers, else the stack is used.
                                         This flag may be set as default on some architectures. */
  cc_last_on_top      = 0x02000000, /**< The last non-register parameter is transmitted on top of
                                         the stack. This is equivalent to the pascal 
                                         calling convention. If this flag is not set, the first
                                         non-register parameter is used (stdcall or cdecl 
                                         calling convention) */
  cc_callee_clear_stk = 0x04000000, /**< The callee clears the stack. This forbids variadic
                                         function calls (stdcall). */
  cc_this_call        = 0x08000000, /**< The first parameter is a this pointer and is transmitted
                                         in a special way. */

  cc_bits             = (0xFF << 24)  /**< the calling convention bits */
} calling_convention;

/* some often used cases: made as defines because firmjni cannot handle two 
   equal enum values. */

/** cdecl calling convention */
#define cc_cdecl_set    (0)
/** stdcall calling convention */
#define cc_stdcall_set  cc_callee_clear_stk
/** fastcall calling convention */
#define cc_fastcall_set (cc_reg_param|cc_callee_clear_stk)

/** Returns the default calling convention for method types. */
unsigned get_default_cc_mask(void);
  
/**
 * check for the CDECL calling convention
 */
#define IS_CDECL(cc_mask)     (((cc_mask) & cc_bits) == cc_cdecl_set)

/**
 * check for the STDCALL calling convention
 */
#define IS_STDCALL(cc_mask)   (((cc_mask) & cc_bits) == cc_stdcall_set)

/**
 * check for the FASTCALL calling convention
 */
#define IS_FASTCALL(cc_mask)  (((cc_mask) & cc_bits) == cc_fastcall_set)

/**
 * Sets the CDECL convention bits.
 */
#define SET_CDECL(cc_mask)    (((cc_mask) & ~cc_bits) | cc_cdecl_set)

/**
 * Set. the STDCALL convention bits.
 */
#define SET_STDCALL(cc_mask)  (((cc_mask) & ~cc_bits) | cc_stdcall_set)

/**
 * Sets the FASTCALL convention bits.
 */
#define SET_FASTCALL(cc_mask) (((cc_mask) & ~cc_bits) | cc_fastcall_set)

/** Returns the calling convention of an entities graph. */
unsigned get_method_calling_convention(const ir_type *method);

/** Sets the calling convention of an entities graph. */
void set_method_calling_convention(ir_type *method, unsigned cc_mask);

/** Returns the number of registers parameters, 0 means default. */
unsigned get_method_n_regparams(ir_type *method);

/** Sets the number of registers parameters, 0 means default. */
void set_method_n_regparams(ir_type *method, unsigned n_regs);

/** Returns true if a type is a method type. */
int   is_Method_type     (const ir_type *method);

/**
 *   @page union_type   Representation of a union (variant) type.
 *
 *   The union type represents union types.  Note that this representation
 *   resembles the C union type.  For tagged variant types like in Pascal or Modula
 *   a combination of a struct and a union type must be used.
 *
 *   - n_types:     Number of unioned types.
 *   - members:     Entities for unioned types.  Fixed length array.
 *                  This is a dynamic list that can be grown with an "add_" function,
 *                  but not shrinked.
 */
/** Creates a new type union. */
ir_type   *new_type_union (ident *name);

/** Creates a new type union with debug information. */
ir_type   *new_d_type_union (ident *name, dbg_info* db);

/* --- manipulate private fields of struct --- */

/** Returns the number of unioned types of this union */
int     get_union_n_members      (const ir_type *uni);

/** Adds a new entity to a union type */
void    add_union_member (ir_type *uni, ir_entity *member);

/** Returns the entity at position pos of a union */
ir_entity *get_union_member (const ir_type *uni, int pos);

/** Returns index of member in uni, -1 if not contained. */
int     get_union_member_index(const ir_type *uni, ir_entity *member);

/** Overwrites a entity at position pos in a union type. */
void    set_union_member (ir_type *uni, int pos, ir_entity *member);

/** Finds member in the list of members and removes it. */
void    remove_union_member (ir_type *uni, ir_entity *member);

/** Returns true if a type is a union type. */
int     is_Union_type          (const ir_type *uni);

/**
 * @page array_type Representation of an array type
 *
 * The array type represents rectangular multi dimensional arrays.
 * The constants representing the bounds must be allocated to
 * get_const_code_irg() by setting current_ir_graph accordingly.
 *
 * - n_dimensions:    Number of array dimensions.
 * - *lower_bound:    Lower bounds of dimensions.  Usually all 0.
 * - *upper_bound:    Upper bounds or dimensions.
 * - *element_type:   The type of the array elements.
 * - *element_ent:    An entity for the array elements to be used for
 *                      element selection with Sel.
 * @todo
 *   Do we need several entities?  One might want
 *   to select a dimension and not a single element in case of multi
 *   dimensional arrays.
 */

/** Create a new type array.
 *
 * Sets n_dimension to dimension and all dimension entries to NULL.
 * Initializes order to the order of the dimensions.
 * The entity for array elements is built automatically.
 * Set dimension sizes after call to constructor with set_* routines.
 */
ir_type *new_type_array         (ident *name, int n_dimensions,
                  ir_type *element_type);

/** Create a new type array with debug information.
 *
 * Sets n_dimension to dimension and all dimension entries to NULL.
 * Initializes order to the order of the dimensions.
 * The entity for array elements is built automatically.
 * Set dimension sizes after call to constructor with set_* routines.
 * A legal array type must have at least one dimension set.
 */
ir_type *new_d_type_array         (ident *name, int n_dimensions,
                  ir_type *element_type, dbg_info* db);

/* --- manipulate private fields of array type --- */

/** Returns the number of array dimensions of this type. */
int   get_array_n_dimensions (const ir_type *array);

/**
 * Allocates Const nodes of mode_Is for one array dimension.
 * Upper bound in Firm is the element next to the last, i.e. [lower,upper[
 */
void  set_array_bounds_int   (ir_type *array, int dimension, int lower_bound,
                                                          int upper_bound);
/**
 * Sets the bounds for one array dimension.
 * Upper bound in Firm is the element next to the last, i.e. [lower,upper[
 */
void  set_array_bounds       (ir_type *array, int dimension, ir_node *lower_bound,
                                                          ir_node *upper_bound);
/** Sets the lower bound for one array dimension, i.e. [lower,upper[ */
void  set_array_lower_bound  (ir_type *array, int dimension, ir_node *lower_bound);

/** Allocates Const nodes of mode_Is for the lower bound of an array
    dimension, i.e. [lower,upper[ */
void  set_array_lower_bound_int (ir_type *array, int dimension, int lower_bound);

/** Sets the upper bound for one array dimension, i.e. [lower,upper[ */
void  set_array_upper_bound  (ir_type *array, int dimension, ir_node *upper_bound);

/** Allocates Const nodes of mode_Is for the upper bound of an array
    dimension, i.e. [lower,upper[. */
void  set_array_upper_bound_int (ir_type *array, int dimension, int upper_bound);

/** Returns true if lower bound != Unknown. */
int       has_array_lower_bound     (const ir_type *array, int dimension);
/** Returns the lower bound of an array. */
ir_node * get_array_lower_bound     (const ir_type *array, int dimension);
/** Works only if bound is Const node with tarval that can be converted to long. */
long      get_array_lower_bound_int (const ir_type *array, int dimension);
/** returns true if lower bound != Unknown */
int       has_array_upper_bound     (const ir_type *array, int dimension);
/** Returns the upper bound of an array. */
ir_node * get_array_upper_bound     (const ir_type *array, int dimension);
/** Works only if bound is Const node with tarval that can be converted to long. */
long      get_array_upper_bound_int (const ir_type *array, int dimension);

/** Sets an array dimension to a specific order. */
void set_array_order (ir_type *array, int dimension, int order);

/** Returns the order of an array dimension. */
int  get_array_order (const ir_type *array, int dimension);

/** Find the array dimension that is placed at order order. */
int find_array_dimension(const ir_type *array, int order);

/** Sets the array element type. */
void  set_array_element_type (ir_type *array, ir_type* tp);

/** Gets the array element type. */
ir_type *get_array_element_type (ir_type *array);

/** Sets the array element entity. */
void  set_array_element_entity (ir_type *array, ir_entity *ent);

/** Get the array element entity. */
ir_entity *get_array_element_entity (const ir_type *array);

/** Returns true if a type is an array type. */
int    is_Array_type(const ir_type *array);

/**
 * @page enumeration_type   Representation of an enumeration type
 *
 * Enumeration types need not necessarily be represented explicitly
 * by Firm types, as the frontend can lower them to integer constants as
 * well.  For debugging purposes or similar tasks this information is useful.
 * The type state layout_fixed is set, if all enumeration constant have
 * there tarvals assigned.  Until then
 *
 * - *const:        The target values representing the constants used to
 *                  represent individual enumerations.
 */

#ifndef _IR_ENUM_CONST_TYPEDEF_
#define _IR_ENUM_CONST_TYPEDEF_
typedef struct ir_enum_const ir_enum_const;
#endif

/** Create a new type enumeration -- set the enumerators independently. */
ir_type   *new_type_enumeration(ident *name, int n_enums);

/** Create a new type enumeration with debug information -- set the enumerators independently. */
ir_type   *new_d_type_enumeration(ident *name, int n_enums, dbg_info *db);

/* --- manipulate fields of enumeration type. --- */

/** Set an enumeration constant to a enumeration type at a given position. */
void set_enumeration_const(ir_type *enumeration, int pos, ident *nameid, tarval *con);

/** Returns the number of enumeration values of this enumeration */
int     get_enumeration_n_enums(const ir_type *enumeration);

/** Returns the enumeration constant at a given position. */
ir_enum_const *get_enumeration_const(const ir_type *enumeration, int pos);

/** Returns the enumeration type owner of an enumeration constant. */
ir_type *get_enumeration_owner(const ir_enum_const *enum_cnst);

/** Sets the enumeration constant value. */
void    set_enumeration_value(ir_enum_const *enum_cnst, tarval *con);

/** Returns the enumeration constant value. */
tarval *get_enumeration_value(const ir_enum_const *enum_cnst);

/** Assign an ident to an enumeration constant. */
void    set_enumeration_nameid(ir_enum_const *enum_cnst, ident *id);

/** Returns the assigned ident of an enumeration constant. */
ident  *get_enumeration_nameid(const ir_enum_const *enum_cnst);

/** Returns the assigned name of an enumeration constant. */
const char *get_enumeration_name(const ir_enum_const *enum_cnst);

/** Returns true if a type is a enumeration type. */
int     is_Enumeration_type(const ir_type *enumeration);

/**
 * @page pointer_type   Representation of a pointer type
 *
 * The mode of the pointer type must be a reference mode.
 *
 * Pointer types:
 * - points_to:      The type of the entity this pointer points to.
 */

/** Creates a new type pointer. */
ir_type *new_type_pointer           (ident *name, ir_type *points_to, ir_mode *ptr_mode);

/** Creates a new type pointer with debug information. */
ir_type *new_d_type_pointer         (ident *name, ir_type *points_to, ir_mode *ptr_mode, dbg_info* db);

/* --- manipulate fields of type_pointer --- */

/** Sets the type to which a pointer points to. */
void  set_pointer_points_to_type (ir_type *pointer, ir_type *tp);

/** Returns the type to which a pointer points to. */
ir_type *get_pointer_points_to_type (ir_type *pointer);

/** Returns true if a type is a pointer type. */
int   is_Pointer_type            (const ir_type *pointer);

/** Returns the first pointer type that has as points_to tp.
 *  Not efficient: O(#types).
 *  If not found returns firm_unknown_type. */
ir_type *find_pointer_type_to_type (ir_type *tp);

/**
 * @page primitive_type Representation of a primitive type
 *
 * Primitive types are types that represent atomic data values that
 * map directly to modes.  They don't have private attributes.  The
 * important information they carry is held in the common mode field.
 */
/** Creates a new primitive type. */
ir_type *new_type_primitive(ident *name, ir_mode *mode);

/** Creates a new primitive type with debug information. */
ir_type *new_d_type_primitive(ident *name, ir_mode *mode, dbg_info* db);

/** Returns true if a type is a primitive type. */
int  is_Primitive_type(const ir_type *primitive);


/**
 * @page none_type The None type
 *
 *  This type is an auxiliary type dedicated to support type analyses.
 *
 *  The none type represents that there is no type.  The type can be used to
 *  initialize fields of type* that actually can not contain a type or that
 *  are initialized for an analysis. There exists exactly one type none.
 *  This type is not on the type list in ir_prog. It is
 *  allocated when initializing the type module.
 *
 *  The following values are set:
 *    - mode:  mode_BAD
 *    - name:  "type_none"
 *    - state: layout_fixed
 *    - size:  0
 */
/** A variable that contains the only none type. */
extern ir_type *firm_none_type;

/** Returns the none type. */
ir_type *get_none_type(void);

/**
 * @page unknown_type  The Unknown type
 *
 *  This type is an auxiliary type dedicated to support type analyses.
 *
 *  The unknown type represents that there could be a type, but it is not
 *  known.  This type can be used to initialize fields before an analysis (not known
 *  yet) or to represent the top of a lattice (could not be determined).  There exists
 *  exactly one type unknown. This type is not on the type list in ir_prog.  It is
 *  allocated when initializing the type module.
 *
 *  The following values are set:
 *    - mode:  mode_ANY
 *    - name:  "type_unknown"
 *    - state: layout_fixed
 *    - size:  0
 */
/** A variable that contains the only unknown type. */
extern ir_type *firm_unknown_type;

/** Returns the unknown type. */
ir_type *get_unknown_type(void);


/**
 *  Checks whether a type is atomic.
 *  @param tp   any type
 *  @return true if type is primitive, pointer or enumeration
 */
int is_atomic_type(const ir_type *tp);

/* --- Support for compound types --- */

/**
 * Gets the number of elements in a Firm compound type.
 *
 * This is just a comfortability function, because structs and
 * classes can often be treated be the same code, but they have
 * different access functions to their members.
 *
 * @param tp  The type (must be struct, union or class).
 *
 * @return Number of members in the compound type.
 */
int get_compound_n_members(const ir_type *tp);

/**
 * Gets the member of a Firm compound type at position pos.
 *
 * @param tp  The type (must be struct, union or class).
 * @param pos The number of the member.
 *
 * @return The member entity at position pos.
 *
 * @see get_compound_n_members() for justification of existence.
 */
ir_entity *get_compound_member(const ir_type *tp, int pos);

/** Returns index of member in tp, -1 if not contained. */
int     get_compound_member_index(const ir_type *tp, ir_entity *member);

/**
 * Checks whether a type is a compound type.
 *
 * @param tp - any type
 *
 * @return true if the type is class, structure, union or array type.
 */
int is_compound_type(const ir_type *tp);

/**
 * Checks, whether a type is a frame type.
 */
int is_frame_type(const ir_type *tp);

/**
 * Checks, whether a type is a value parameter type.
 */
int is_value_param_type(const ir_type *tp);

/**
 * Checks, whether a type is a lowered type.
 */
int is_lowered_type(const ir_type *tp);

/**
 * Makes a new frame type. Frame types are class types,
 * so all class access functions work.
 * Frame types are not in the global list of types.
 */
ir_type *new_type_frame(ident *name);

/**
 * Sets a lowered type for a type. This sets both associations
 * and marks lowered_type as a "lowered" one.
 */
void set_lowered_type(ir_type *tp, ir_type *lowered_type);

/**
 * Gets the lowered/unlowered type of a type or NULL if this type
 * has no lowered/unlowered one.
 */
ir_type *get_associated_type(const ir_type *tp);

/**
 * Allocate an area of size bytes aligned at alignment 
 * at the start or the end of a frame type.
 * The frame type must already have a fixed layout.
 *
 * @param frame_type a frame type
 * @param size       the size of the entity
 * @param alignment  the alignment of the entity
 * @param at_start   if true, put the area at the frame type's start, else at end
 *
 * @return the entity representing the area
 */
ir_entity *frame_alloc_area(ir_type *frame_type, int size, int alignment, int at_start);

/*-----------------------------------------------------------------*/
/** Debug aides                                                   **/
/*-----------------------------------------------------------------*/

/**
 *  Outputs a unique number for this type if libfirm is compiled for
 *  debugging, (configure with --enable-debug) else returns the address
 *  of the type cast to long.
 */
long get_type_nr(const ir_type *tp);

#endif /* _FIRM_TR_TYPE_H_ */
