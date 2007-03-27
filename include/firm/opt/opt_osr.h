/**
 * Project:     libFIRM
 * File name:   ir/opt/opt_osr.h
 * Purpose:     Operator Strength Reduction,
 *              Keith D. Cooper, L. Taylor Simpson, Christopher A. Vick
 * Author:      Michael Beck
 * Modified by:
 * Created:     12.5.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _OPT_OSR_H_
#define _OPT_OSR_H_

#include "firm_types.h"

/** Possible flags for the Operator Scalar Replacement. */
typedef enum osr_flags {
	osr_flag_none               = 0,  /**< no additional flags */
	osr_flag_lftr_with_ov_check = 1,  /**< do linear function test replacement
	                                       only if no overflow can occur. */
	osr_flag_ignore_x86_shift   = 2   /**< ignore Multiplications by 2, 4, 8 */
} osr_flags;

/* FirmJNI cannot handle identical enum values... */

/** default setting */
#define osr_flag_default osr_flag_lftr_with_ov_check

/**
 * Do the Operator Scalar Replacement optimization and linear
 * function test replacement for loop control.
 * Can be switched off using the set_opt_strength_red() flag.
 * In that case, only remove_phi_cycles() is executed.
 *
 * @param irg    the graph which should be optimized
 * @param flags  set of osr_flags
 *
 * The linear function replacement test is controlled by the flags.
 * If the osr_flag_lftr_with_ov_check is set, the replacement is only
 * done if do overflow can occur.
 * Otherwise it is ALWAYS done which might be insecure.
 *
 * For instance:
 *
 * for (i = 0; i < 100; ++i)
 *
 * might be replaced by
 *
 * for (i = 0; i < 400; i += 4)
 *
 * But
 *
 * for (i = 0; i < 0x7FFFFFFF; ++i)
 *
 * will not be replaced by
 *
 * for (i = 0; i < 0xFFFFFFFC; i += 4)
 *
 * because of overflow.
 *
 * More bad cases:
 *
 * for (i = 0; i <= 0xF; ++i)
 *
 * will NOT be transformed into
 *
 * for (i = 0xFFFFFFF0; i <= 0xFFFFFFFF; ++i)
 *
 * although here is no direct overflow. The OV occurs when the ++i
 * is executed (and would created an endless loop here!).
 *
 * For the same reason, a loop
 *
 * for (i = 0; i <= 9; i += x)
 *
 * will NOT be transformed because we cannot estimate whether an overflow
 * might happen adding x.
 *
 * Note that i < a + 400 is also not possible with the current implementation
 * although this might be allowed by other compilers...
 *
 * Note further that tests for equality can be handled some simpler (but are not
 * implemented yet).
 *
 * This algorithm destroys the link field of nodes.
 */
void opt_osr(ir_graph *irg, unsigned flags);

/**
 * Removes useless Phi cycles, i.e cycles of Phi nodes with only one
 * non-Phi node.
 * This is automatically done in opt_osr(), so there is no need to call it
 * additionally.
 *
 * @param irg    the graph which should be optimized
 *
 * This algorithm destroys the link field of nodes.
 */
void remove_phi_cycles(ir_graph *irg);

#endif /* _OPT_OSR_H_ */

