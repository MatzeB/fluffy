/*
 * Project:     libFIRM
 * File name:   ir/tr/entity.h
 * Purpose:     Representation of all program known entities.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file entity.h
 *
 * Entities represent all program known objects.
 *
 * @author Martin Trapp, Christian Schaefer
 * @author Goetz Lindenmaier
 *
 * An entity is the representation of program known objects in Firm.
 * The primary concept of entities is to represent members of complex
 * types, i.e., fields and methods of classes.  As not all programming
 * language model all variables and methods as members of some class,
 * the concept of entities is extended to cover also local and global
 * variables, and arbitrary procedures.
 *
 * An entity always specifies the type of the object it represents and
 * the type of the object it is a part of, the owner of the entity.
 * Originally this is the type of the class of which the entity is a
 * member.
 * The owner of local variables is the procedure they are defined in.
 * The owner of global variables and procedures visible in the whole
 * program is a universally defined class type "GlobalType".  The owner
 * of procedures defined in the scope of an other procedure is the
 * enclosing procedure.
 *
 * @link ir_entity
 */
#ifndef _FIRM_TR_ENTITY_H_
#define _FIRM_TR_ENTITY_H_

#include "firm_types.h"
#include "dbginfo.h"

#include "tr_inheritance.h"

/*-----------------------------------------------------------------*/
/* ENTITY                                                          */
/*-----------------------------------------------------------------*/

/**
 * @page ir_entity  Representation of an program entity.
 *
 * The type ir_entity is an abstract data type to represent program entities.
 * If contains the following attributes:
 *
 *   - owner:      A compound type this entity is a part of.
 *   - type:       The type of this entity.
 *   - name:       The string that represents this entity in the source program.
 *   - allocation: A flag saying whether the entity is dynamically or statically
 *                 allocated (values: dynamic_allocated,  static_allocated,
 *                 automatic_allocated).
 *   - visibility: A flag indicating the visibility of this entity (values: local,
 *                 external_visible,  external_allocated)
 *   - variability: A flag indicating the variability of this entity (values:
 *                  uninitialized, initialized, part_constant, constant)
 *   - volatility: @@@  
 *   - offset:     The offset of the entity within the compound object in bytes.  Only set
 *                 if the owner in the state "layout_fixed".
 *   - offset_bits_remainder:   The offset bit remainder of a bitfield entity (in a compound)
 *                 in bits.  Only set if the owner in the state "layout_fixed".
 *   - overwrites: A list of entities overwritten by this entity.  This list is only
 *                 existent if the owner of this entity is a class.  The members in
 *                 this list must be entities of super classes.
 *   - overwrittenby: A list of entities that overwrite this entity.  This list is only
 *                 existent if the owner of this entity is a class.  The members in
 *                 this list must be entities of sub classes.
 *   - link:       A void* to associate some additional information with the entity.
 *   - irg:        If the entity is a method this is the ir graph that represents the
 *                 code of the method.
 *   - peculiarity: The peculiarity of the entity.  If the entity is a method this
 *                 indicates whether the entity represents
 *                 a real method or whether it only exists to describe an interface.
 *                 In that case there nowhere exists code for this entity and this entity
 *                 is never dynamically used in the code.
 *                 Values: description, existent.  Default: existent.
 *   - visited:    visited flag.  Master flag is type_visited.
 *
 * These fields can only be accessed via access functions.
 *
 * @see  ir_type
 */

/* to resolve recursion between entity.h and type.h */
/** the type of an entity */
#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
typedef struct ir_entity ir_entity, entity;
#endif

/**
 * Creates a new entity.
 *
 * Automatically inserts the entity as a member of owner.
 * Entity is automatic_allocated and uninitialized except if the type
 * is type_method, then it is static_allocated and constant.  The constant
 * value is a pointer to the method.
 * Visibility is local, offset -1, and it is not volatile.
 */
ir_entity     *new_entity(ir_type *owner, ident *name, ir_type *tp);

/**
 * Creates a new entity.
 *
 * Automatically inserts the entity as a member of owner.
 * The entity is automatic allocated and uninitialized except if the type
 * is type_method, then it is static allocated and constant.  The constant
 * value is a pointer to the method.
 * Visibility is local, offset -1, and it is not volatile.
 */
ir_entity     *new_d_entity(ir_type *owner, ident *name, ir_type *tp, dbg_info *db);

/**
 * Copies the entity if the new_owner is different from the
 * owner of the old entity,  else returns the old entity.
 *
 * Automatically inserts the new entity as a member of owner.
 * Resets the overwrites/overwritten_by fields.
 * Keeps the old atomic value.  
 *   @@@ Maybe we should change this.  If peculiarity of a method 
 *       is existent, we should add a new SymConst that points to 
 *       itself and not to the origin.  Right now we have to change
 *       the peculiarity and then set a new atomic value by hand. 
 */
ir_entity     *copy_entity_own(ir_entity *old, ir_type *new_owner);

/**
 * Copies the entity if the new_name is different from the
 * name of the old entity, else returns the old entity.
 *
 * Automatically inserts the new entity as a member of owner.
 * The mangled name ld_name is set to NULL.
 * Overwrites relation is copied from old. 
 */
ir_entity     *copy_entity_name(ir_entity *old, ident *new_name);

/**
 * Frees the entity.
 *
 * The owner will still contain the pointer to this
 * entity, as well as all other references!
 */
void        free_entity(ir_entity *ent);

/** Returns the name of an entity. */
const char *get_entity_name(const ir_entity *ent);

/** Returns the ident of an entity. */
ident      *get_entity_ident(const ir_entity *ent);

/** Sets the ident of the entity. */
void        set_entity_ident(ir_entity *ent, ident *id);

/** Returns the mangled name of the entity.
 *
 * If the mangled name is set it returns the existing name.
 * Else it generates a name with mangle_entity()
 * and remembers this new name internally.
 */
ident      *get_entity_ld_ident(ir_entity *ent);

/** Sets the mangled name of the entity. */
void        set_entity_ld_ident(ir_entity *ent, ident *ld_ident);

/** Returns the mangled name of the entity as a string. */
const char *get_entity_ld_name(ir_entity *ent);

/** Returns the owner of the entity. */
ir_type    *get_entity_owner(ir_entity *ent);

/** Sets the owner field in entity to owner.  Don't forget to add
   ent to owner!! */
void        set_entity_owner(ir_entity *ent, ir_type *owner);

/** Returns the type of an entity. */
ir_type  *get_entity_type(ir_entity *ent);

/** Sets the type of an entity. */
void      set_entity_type(ir_entity *ent, ir_type *tp);

/** The allocation type. */
typedef enum {
  allocation_automatic, /**< The entity is allocated during runtime, implicitly
                             as component of a compound type.   This is the default. */
  allocation_parameter, /**< The entity is a parameter.  It is also automatic allocated.
                             We distinguish the allocation of parameters from the allocation
                             of local variables as their placement depends on the calling
                             conventions. */
  allocation_dynamic,   /**< The entity is allocated during runtime, explicitly
                             by an Alloc node. */
  allocation_static     /**< The entity is allocated statically.  We can use a
                             Const as address of the entity.  This is the default for methods. */
} ir_allocation;

/** Returns the allocation type of an entity. */
ir_allocation get_entity_allocation(const ir_entity *ent);

/** Sets the allocation type of an entity. */
void           set_entity_allocation(ir_entity *ent, ir_allocation al);

/** Return the name of the allocation type. */
const char *get_allocation_name(ir_allocation vis);

/** Returns the visibility of an entity. */
ir_visibility get_entity_visibility(const ir_entity *ent);

/** Sets the visibility of an entity. */
void       set_entity_visibility(ir_entity *ent, ir_visibility vis);

/** Return the name of the visibility */
const char *get_visibility_name(ir_visibility vis);

/** This enumeration flags the variability of entities. */
typedef enum {
  variability_uninitialized,    /**< The content of the entity is completely unknown. Default. */
  variability_initialized,      /**< After allocation the entity is initialized with the
                                     value given somewhere in the entity. */
  variability_part_constant,    /**< For entities of compound types.
                                     The members of the entity are mixed constant,
                                     initialized or uninitialized. */
  variability_constant          /**< The entity is constant. */
} ir_variability;

/** Returns the variability of an entity. */
ir_variability get_entity_variability(const ir_entity *ent);

/** Sets the variability of an entity. */
void           set_entity_variability(ir_entity *ent, ir_variability var);

/** Return the name of the variability. */
const char *get_variability_name(ir_variability var);

/** This enumeration flags the volatility of entities. */
typedef enum {
  volatility_non_volatile,    /**< The entity is not volatile. Default. */
  volatility_is_volatile      /**< The entity is volatile */
} ir_volatility;

/** Returns the volatility of an entity. */
ir_volatility get_entity_volatility(const ir_entity *ent);

/** Sets the volatility of an entity. */
void          set_entity_volatility(ir_entity *ent, ir_volatility vol);

/** Return the name of the volatility. */
const char *get_volatility_name(ir_volatility var);

/** This enumeration flags the stickyness of an entity. */
typedef enum {
  stickyness_unsticky,          /**< The entity can be removed from
                                   the program, unless contraindicated
                                   by other attributes. Default. */
  stickyness_sticky             /**< The entity must remain in the
                                   program in any case. */
} ir_stickyness;

/** Get the entity's stickyness. */
ir_stickyness get_entity_stickyness(const ir_entity *ent);

/** Set the entity's stickyness. */
void          set_entity_stickyness(ir_entity *ent, ir_stickyness stickyness);

/** Returns the offset of an entity (in a compound) in bytes. Only set if layout = fixed. */
int       get_entity_offset(const ir_entity *ent);

/** Sets the offset of an entity (in a compound) in bytes. */
void      set_entity_offset(ir_entity *ent, int offset);

/** Returns the offset bit remainder of a bitfield entity (in a compound) in bits. Only set if layout = fixed. */
unsigned char get_entity_offset_bits_remainder(const ir_entity *ent);

/** Sets the offset bit remainder of a bitfield entity (in a compound) in bits. */
void      set_entity_offset_bits_remainder(ir_entity *ent, unsigned char offset);

/** Returns the stored intermediate information. */
void *get_entity_link(const ir_entity *ent);

/** Stores new intermediate information. */
void set_entity_link(ir_entity *ent, void *l);

/* -- Fields of method entities -- */
/** The entity knows the corresponding irg if the entity is a method.
   This allows to get from a Call to the called irg.
   Only entities of peculiarity "existent" can have a corresponding irg,
   else the field is fixed to NULL.  (Get returns NULL, set asserts.) */
ir_graph *get_entity_irg(const ir_entity *ent);
void      set_entity_irg(ir_entity *ent, ir_graph *irg);

/** Gets the entity vtable number. */
unsigned get_entity_vtable_number(const ir_entity *ent);

/** Sets the entity vtable number. */
void     set_entity_vtable_number(ir_entity *ent, unsigned vtable_number);

/** Return the peculiarity of an entity. */
ir_peculiarity get_entity_peculiarity(const ir_entity *ent);

/** Sets the peculiarity of an entity. */
void           set_entity_peculiarity(ir_entity *ent, ir_peculiarity pec);

/** Checks if an entity cannot be overridden anymore. */
int is_entity_final(const ir_entity *ent);

/** Sets/resets the final flag of an entity. */
void set_entity_final(ir_entity *ent, int final);

/** Checks if an entity is compiler generated. */
int is_entity_compiler_generated(const ir_entity *ent);

/** Sets/resets the compiler generated flag. */
void set_entity_compiler_generated(ir_entity *ent, int flag);

/**
 * The state of the address_taken flag.
 */
typedef enum {
	ir_address_not_taken     = 0,  /**< The address is NOT taken. */
	ir_address_taken_unknown = 1,  /**< The state of the address taken flag is unknown. */
	ir_address_taken         = 2   /**< The address IS taken. */
} ir_address_taken_state;

/** Return the state of the address taken flag of an entity. */
ir_address_taken_state get_entity_address_taken(const ir_entity *ent);

/** Sets/resets the state of the address taken flag of an entity. */
void set_entity_address_taken(ir_entity *ent, ir_address_taken_state flag);

/** Return the name of the address_taken state. */
const char *get_address_taken_state_name(ir_address_taken_state state);

/* -- Representation of constant values of entities -- */
/** 
 * Returns true if the the node is representable as code on
 * const_code_irg. 
 *
 * @deprecated This function is not used by libFirm and stays here
 *             only as a helper for the old Jack frontend.
 */
int      is_irn_const_expression(ir_node *n);

/**
 * Copies a Firm subgraph that complies to the restrictions for
 * constant expressions to current_block in current_ir_graph.
 *
 * @param dbg  debug info for all newly created nodes
 * @param n    the node
 *
 * Set current_ir_graph to get_const_code_irg() to generate a constant
 * expression.
 */
ir_node *copy_const_value(dbg_info *dbg, ir_node *n);

/* Set has no effect for existent entities of type method. */
ir_node *get_atomic_ent_value(ir_entity *ent);
void     set_atomic_ent_value(ir_entity *ent, ir_node *val);

/**
 * The following type describes a path to a leave in the compound graph.
 * Node 0 in the path must be an entity of type tp given in the constructor.  If
 * the type of this element is compound, the path node 1 is an element of the type
 * of node 0 an so forth, until an entity of atomic type is reached.
 */
#ifndef _COMPOUND_GRAPH_PATH_TYPEDEF_
#define _COMPOUND_GRAPH_PATH_TYPEDEF_
typedef struct compound_graph_path compound_graph_path;
#endif /* _COMPOUND_GRAPH_PATH_TYPEDEF_ */

/** Creates a new compound graph path. */
compound_graph_path *new_compound_graph_path(ir_type *tp, int length);

/** Returns non-zero if an object is a compound graph path */
int     is_compound_graph_path(const void *thing);

/** Frees a graph path object */
void    free_compound_graph_path (compound_graph_path *gr);

/** Returns the length of a graph path */
int     get_compound_graph_path_length(const compound_graph_path *gr);

ir_entity *get_compound_graph_path_node(const compound_graph_path *gr, int pos);
void    set_compound_graph_path_node(compound_graph_path *gr, int pos, ir_entity *node);
int     get_compound_graph_path_array_index(const compound_graph_path *gr, int pos);
void    set_compound_graph_path_array_index(compound_graph_path *gr, int pos, int index);

/** Checks whether the path up to pos is correct. If the path contains a NULL, 
 *  assumes the path is not complete and returns non-zero. */
int is_proper_compound_graph_path(compound_graph_path *gr, int pos);

/* A value of a compound entity is a pair of a value and the description of the
   corresponding access path to the member of the compound.  */
void     add_compound_ent_value_w_path(ir_entity *ent, ir_node *val, compound_graph_path *path);
void     set_compound_ent_value_w_path(ir_entity *ent, ir_node *val, compound_graph_path *path, int pos);
/** Returns the number of constant values needed to initialize the entity. 
 * 
 *  Asserts if the entity has variability_uninitialized. 
 * */
int      get_compound_ent_n_values(ir_entity *ent);
/** Returns a constant value given the position. */
ir_node *get_compound_ent_value(ir_entity *ent, int pos);
/** Returns the access path for value at position pos. */
compound_graph_path *get_compound_ent_value_path(ir_entity *ent, int pos);
/** Returns a constant value given the access path. 
 *  The path must contain array indices for all array element entities. */
ir_node *get_compound_ent_value_by_path(ir_entity *ent, compound_graph_path *path);

/** Removes all constant entries where the path ends at value_ent. Does not
   free the memory of the paths.  (The same path might be used for several
   constant entities. */
void     remove_compound_ent_value(ir_entity *ent, ir_entity *value_ent);

/* Some languages support only trivial access paths, i.e., the member is a
   direct, atomic member of the constant entities type. In this case the
   corresponding entity can be accessed directly.  The following functions
   allow direct access. */

/** Generates a Path with length 1. 
    Beware: Has a bad runtime for array elements (O(|array|) and should be
    avoided there. Use add_compound_ent_value_w_path() instead and create
    the path manually. */
void     add_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member);

/** Returns the last member in the path */
ir_entity  *get_compound_ent_value_member(ir_entity *ent, int pos);

/** Sets the path at pos 0 */
void     set_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member, int pos);

/** Initializes the entity ent which must be of a one dimensional
   array type with the values given in the values array.
   The array must have a lower and an upper bound.  Keeps the
   order of values. Does not test whether the number of values
   fits into the given array size.  Does not test whether the
   values have the proper mode for the array. */
void set_array_entity_values(ir_entity *ent, tarval **values, int num_vals);

/**
 * Return the offset in bits from the last byte address.
 *
 * This requires that the layout of all concerned types is fixed.
 *
 * @param ent Any entity of compound type with at least pos initialization values. 
 * @param pos The position of the value for which the offset is requested. 
 */
int get_compound_ent_value_offset_bit_remainder(ir_entity *ent, int pos);

/** Return the overall offset of value at position pos in bytes. 
 *
 * This requires that the layout of all concerned types is fixed.
 * Asserts if bit offset is not byte aligned. 
 *
 * @param ent Any entity of compound type with at least pos initialization values. 
 * @param pos The position of the value for which the offset is requested. 
 */
int  get_compound_ent_value_offset_bytes(ir_entity *ent, int pos);

/* --- Fields of entities with a class type as owner --- */
/* Overwrites is a field that specifies that an access to the overwritten
   entity in the supertype must use this entity.  It's a list as with
   multiple inheritance several entities can be overwritten.  This field
   is mostly useful for method entities.
   If a Sel node selects an entity that is overwritten by other entities it
   must return a pointer to the entity of the dynamic type of the pointer
   that is passed to it.  Lowering of the Sel node must assure this.
   Overwrittenby is the inverse of overwrites.  Both add routines add
   both relations, they only differ in the order of arguments. */
void    add_entity_overwrites   (ir_entity *ent, ir_entity *overwritten);
int     get_entity_n_overwrites (ir_entity *ent);
int     get_entity_overwrites_index(ir_entity *ent, ir_entity *overwritten);
ir_entity *get_entity_overwrites   (ir_entity *ent, int pos);
void    set_entity_overwrites   (ir_entity *ent, int pos, ir_entity *overwritten);
void    remove_entity_overwrites(ir_entity *ent, ir_entity *overwritten);

void    add_entity_overwrittenby   (ir_entity *ent, ir_entity *overwrites);
int     get_entity_n_overwrittenby (ir_entity *ent);
int     get_entity_overwrittenby_index(ir_entity *ent, ir_entity *overwrites);
ir_entity *get_entity_overwrittenby   (ir_entity *ent, int pos);
void    set_entity_overwrittenby   (ir_entity *ent, int pos, ir_entity *overwrites);
void    remove_entity_overwrittenby(ir_entity *ent, ir_entity *overwrites);

/**
 *   Checks whether a pointer points to an entity.
 *
 *   @param thing     an arbitrary pointer
 *
 *   @return
 *       true if the thing is an entity, else false
 */
int is_entity (const void *thing);

/** Returns true if the type of the entity is a primitive, pointer
 * enumeration or method type. 
 *
 * @Note This is a different classification than from is_primitive_type().
 */
int is_atomic_entity(ir_entity *ent);
/** Returns true if the type of the entity is a class, structure,
   array or union type. */
int is_compound_entity(ir_entity *ent);
/** Returns true if the type of the entity is a Method type. */
int is_method_entity(ir_entity *ent);

/** Returns non-zero if ent1 and ent2 have are equal except for their owner.
   Two entities are equal if
    - they have the same type (the same C-struct)
    - ...?
*/
int equal_entity(ir_entity *ent1, ir_entity *ent2);

/** Outputs a unique number for this entity if libfirm is compiled for
 *  debugging, (configure with --enable-debug) else returns the address
 *  of the type cast to long.
 */
long get_entity_nr(const ir_entity *ent);

/** Returns the entities visited count. */
unsigned long get_entity_visited(ir_entity *ent);

/** Sets the entities visited count. */
void        set_entity_visited(ir_entity *ent, unsigned long num);

/** Sets visited field in entity to entity_visited. */
void        mark_entity_visited(ir_entity *ent);

/** Returns true if this entity was visited. */
int        entity_visited(ir_entity *ent);

/** Returns true if this entity was not visited. */
int        entity_not_visited(ir_entity *ent);

/** 
 * Returns the mask of the additional entity properties. 
 * The properties are automatically inherited from the irg if available
 * or from the method type if they were not set using 
 * set_entity_additional_properties() or
 * set_entity_additional_property().
 */
unsigned get_entity_additional_properties(ir_entity *ent);

/** Sets the mask of the additional graph properties. */
void set_entity_additional_properties(ir_entity *ent, unsigned property_mask);

/** Sets one additional graph property. */
void set_entity_additional_property(ir_entity *ent, mtp_additional_property flag);

/** Returns the class type that this type info entity represents or NULL
    if ent is no type info entity. */
ir_type *get_entity_repr_class(const ir_entity *ent);

/**
 * @page unknown_entity  The Unknown entity
 * 
 *  This entity is an auxiliary entity dedicated to support analyses.
 *
 *  The unknown entity represents that there could be an entity, but it is not
 *  known.  This entity can be used to initialize fields before an analysis (not known
 *  yet) or to represent the top of a lattice (could not be determined).  There exists 
 *  exactly one entity unknown. This entity has as owner and as type the unknown type. It is 
 *  allocated when initializing the entity module. 
 *
 *  The entity can take the role of any entity, also methods.  It returns default
 *  values in these cases. 
 *
 *  The following values are set:
 *
 * - name          = "unknown_entity"
 * - ld_name       = "unknown_entity"
 * - owner         = unknown_type
 * - type          = unknown_type
 * - allocation    = allocation_automatic
 * - visibility    = visibility_external_allocated
 * - offset        = -1
 * - variability   = variability_uninitialized
 * - value         = SymConst(unknown_entity)
 * - values        = NULL
 * - val_paths     = NULL
 * - peculiarity   = peculiarity_existent
 * - volatility    = volatility_non_volatile
 * - stickyness    = stickyness_unsticky
 * - ld_name       = NULL
 * - overwrites    = NULL
 * - overwrittenby = NULL
 * - irg           = NULL
 * - link          = NULL
 */
/* A variable that contains the only unknown entity. */
extern ir_entity *unknown_entity; 

/** Returns the @link unknown_entity unknown entity @endlink. */
ir_entity *get_unknown_entity(void);

/** Encodes how a pointer parameter is accessed. */
typedef enum acc_bits {
  ptr_access_none  = 0,                                 /**< no access */
  ptr_access_read  = 1,                                 /**< read access */
  ptr_access_write = 2,                                 /**< write access */
  ptr_access_rw    = ptr_access_read|ptr_access_write,  /**< read AND write access */
  ptr_access_store = 4,                                 /**< the pointer is stored */
  ptr_access_all   = ptr_access_rw|ptr_access_store     /**< all possible access */
} ptr_access_kind;

#define IS_READ(a)     ((a) & ptr_access_read)
#define IS_WRITTEN(a)  ((a) & ptr_access_write)
#define IS_STORED(a)   ((a) & ptr_access_store)

/** 
 * Supported image sections. 
 * Currently only methods can be placed in different sections. 
 */
typedef enum {
  section_text,           /**< The code segment. This is the default for methods. */
  section_constructors    /**< The constructor section. */
} ir_img_section;

/** Returns the section of a method. */
ir_img_section get_method_img_section(const ir_entity *method);

/** Sets the section of a method. */
void set_method_img_section(ir_entity *method, ir_img_section section);

#endif /* _FIRM_TR_ENTITY_H_ */

