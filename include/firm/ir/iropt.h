/*
 * Project:     libFIRM
 * File name:   ir/ir/iropt.h
 * Purpose:     iropt --- optimizations of an ir node.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file iropt.h
 *
 * Declarations for optimizations of an ir node.
 *
 * @author Martin Trapp, Christian Schaefer
 */
#ifndef _FIRM_IR_IROPT_H_
#define _FIRM_IR_IROPT_H_

#include "firm_types.h"

/**
 * The Floating point model.
 *
 * Several basic properties are defined:
 * - fp_explicit_rounding
 * - fp_strict_algebraic
 * - fp_contradictions
 * - fp_strict_eval_order
 * - fp_exceptions
 * - fp_environment_access
 *
 * From those basic properties three general models are defined,
 * compatible to the VC8 compiler:
 * - fp_model_precise:
 *     Default mode. Associative and distributive law forbidden unless a transformation
 *     is guaranteed to produce the same result.
 *     No FPU environment access. No FP exception semantics.
 * - fp_model_strict:
 *     Slowest mode. Additionally to fp_model_precise allows correct handling of
 *     FP exceptions and FPU environment access.
 * - fp_model_fast: 
 *     Fastest mode. Associative and distributive law allowed at the expense
 *     of floating point accuracy and correctness. Explicit rounding is disabled.
 */
typedef enum _fp_model_t {
  fp_explicit_rounding  =  1,  /**< Explicit rounding at assignments, typecasts, return
                                    and function calls. Conv nodes may NOT be removed, even
                                    if they look useless. */
  fp_strict_algebraic   =  2,  /**< Strict adherence to non-associative and non-distributive
                                    algebra unless the same result is guaranteed. */
  fp_contradictions     =  4,  /**< FP contradictions are enabled. Only for backend. */
  fp_strict_eval_order  =  8,  /**< FP instructions must be strict evaluated in given order. */
  fp_exceptions         = 16,  /**< FP exceptions are supported. No reordering that changes
                                    the exception flow are allowed. Backends must generate
                                    synchronized exception code. */
  fp_environment_access = 32,  /**< FPU environment can be accessed. Even Constant folding
                                    cannot be done. */

  /** Precise floating point model. Default. */
  fp_model_precise = fp_explicit_rounding|fp_strict_algebraic|fp_contradictions,  
  /** Strict floating point model. */
  fp_model_strict  = fp_explicit_rounding|fp_strict_algebraic|fp_strict_eval_order|
                     fp_exceptions|fp_environment_access,
  /** Fast floating point model. */
  fp_model_fast    = fp_contradictions,
} fp_model_t;

/** If the expression referenced can be evaluated statically
 *  computed_value returns a tarval representing the result.
 *  Else returns tarval_bad. */
tarval *computed_value(ir_node *n);

/** Applies all optimizations to n that are expressible as a pattern
 *  in Firm, i.e., they need not a walk of the graph.
 *  Returns a better node for n.  Does not free n -- other nodes could
 *  reference n.
 *
 *  An equivalent optimization is applied in the constructors defined in
 *  ircons.ch.  There n is freed if a better node could be found.
 */
ir_node *optimize_in_place(ir_node *n);

#endif /* _FIRM_IR_IROPT_H_ */

