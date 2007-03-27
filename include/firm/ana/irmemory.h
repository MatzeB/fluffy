/*
 * Project:     libFIRM
 * File name:   ir/ana/irmemory.h
 * Purpose:     Memory disambiguator
 * Author:      Michael Beck
 * Modified by:
 * Created:     27.12.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006-2007 Universit�t Karlsruhe
 * License:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRM_MEMORY_H
#define _FIRM_MEMORY_H

#include "firm_types.h"

/** The alias relation of two memory addresses. */
typedef enum {
	no_alias,       /**< No alias. */
	may_alias,      /**< Unknown state, may alias. */
	sure_alias      /**< Sure alias. */
} ir_alias_relation;

/** The state of the address taken flags. */
typedef enum {
	ir_address_taken_not_computed, /**< Address taken flag is not computed. */
	ir_address_taken_computed      /**< Address taken flag is computed. */
} ir_address_taken_computed_state;

/** Possible options for the memory disambiguator. */
typedef enum {
	aa_opt_no_opt              = 0,  /**< no options: most conservative */
	aa_opt_type_based          = 1,  /**< use type based alias analysis: strict typed source language */
	aa_opt_byte_type_may_alias = 2,  /**< if type based analysis is enabled: bytes types may alias other types */
	aa_opt_no_alias            = 4,  /**< two addresses NEVER alias, use with CAUTION (gcc -fno-alias) */
	aa_opt_inherited           = 128 /**< only for implementation: options from a graph are inherited from global */
} disambuigator_options;

/**
 * A source language specific memory disambiguator function.
 * Called by get_alias_relation().
 */
typedef ir_alias_relation (*DISAMBIGUATOR_FUNC)(
	ir_graph *irg, 
	ir_node *adr1, ir_mode *mode1,
	ir_node *adr2, ir_mode *mode2);

/**
 * Determine the alias relation between two addresses.
 *
 * @param irg     The current graph.
 * @param adr1    The first address.
 * @param mode1   The mode of the first memory access.
 * @param adr2    The second address.
 * @param mode2   The mode of the second memory access.
 *
 * The memory disambiguator tries to determine the alias state between
 * two memory addresses. The following rules are used:
 *
 * - variables from different segments never alias (R1)
 *   - a global variable and a local one never alias (R1 b)
 *   - a global variable and a TLS one never alias (R1 c)
 *   - a local variable and a TLS one never alias (R1 d)
 *   - a local variable and a parameter never alias (R1 e)
 * - two different variables never alias (R2)
 * - if one is a variable which address has never taken
 *   there is no alias (R3)
 * - if two memory addresses have the same base and there offsets
 *   do not describe overlapping regions there is no alias (R4)
 * - if opt_strong_typed is set and both addresses describe entities,
 *   different types never alias (R5)
 *
 * If none of these rules apply, the points-to framework must be
 * interrogated to detect the alias relation.
 */
ir_alias_relation get_alias_relation(
	ir_graph *irg, 
	ir_node *adr1, ir_mode *mode1,
	ir_node *adr2, ir_mode *mode2);

/**
 * Set a source language specific memory disambiguator function.
 *
 * @param func  The callback.
 */
void set_language_memory_disambiguator(DISAMBIGUATOR_FUNC func);

/**
 * Initialize the relation cache.
 */
void mem_disambig_init(void);

/*
 * Determine the alias relation between two addresses and
 * cache the result.
 *
 * @param irg     The current graph.
 * @param adr1    The first address.
 * @param mode1   The mode of the first memory access.
 * @param adr2    The second address.
 * @param mode2   The mode of the second memory access.
 * @param options Additional options.
 *
 * @see get_alias_relation()
 */
ir_alias_relation get_alias_relation_ex(
	ir_graph *irg, 
	ir_node *adr1, ir_mode *mode1,
	ir_node *adr2, ir_mode *mode2);

/**
 * Free the relation cache.
 */
void mem_disambig_term(void);

/**
 * Returns the current address taken state of the graph.
 */
ir_address_taken_computed_state get_irg_address_taken_state(const ir_graph *irg);

/**
 * Sets the current address taken state of the graph.
 *
 * @param irg    the graph
 * @param state  the new state
 */
void set_irg_address_taken_state(ir_graph *irg, ir_address_taken_computed_state state);

/**
 * Assure that the address taken flag is computed for the given graph.
 *
 * This is an intraprocedural analysis that computes the address_taken state
 * for all local variables.
 *
 * Note that this is a conservative estimation that by no Firm transformation
 * can be invalidated, so it's only recomputed if manually triggered by calling
 * set_irg_address_taken_state(irg, ir_address_taken_not_computed).
 * Even then the information is not cleaned from the variables, call 
 * assure_irg_address_taken_computed() again for recomputation.
 */
void assure_irg_address_taken_computed(ir_graph *irg);

/**
 * Returns the current address taken state of the globals.
 */
ir_address_taken_computed_state get_irp_globals_address_taken_state(void);

/**
 * Sets the current address taken state of the globals.
 *
 * @param state  the new state
 */
void set_irp_globals_address_taken_state(ir_address_taken_computed_state state);

/**
 * Assure that the address taken flag is computed for the global and TLS entities (variables).
 *
 * This is an interprocedural analysis that computes the address_taken state
 * for all global and TLS variables.
 *
 * Note that this is a conservative estimation that by no Firm transformation
 * can be invalidated, so it's only recomputed if manually triggered by calling
 * set_irp_globals_address_taken_state(ir_address_taken_not_computed).
 * Even then the information is not cleaned from the variables, call 
 * assure_irp_globals_address_taken_computed() again for recomputation.
 */
void assure_irp_globals_address_taken_computed(void);

/**
 * Get the memory disambiguator options for a graph.
 *
 * @param irg  the graph
 */
unsigned get_irg_memory_disambiguator_options(ir_graph *irg);

/**
 * Set the memory disambiguator options for a graph.
 *
 * @param irg      the graph
 * @param option   a set of options
 */
void set_irg_memory_disambiguator_options(ir_graph *irg, unsigned options);

/**
 * Set the global disambiguator options for all graphs not having local options.
 *
 * @param option   a set of options
 */
void set_irp_memory_disambiguator_options(unsigned options);

#endif /* _FIRM_MEMORY_H */
