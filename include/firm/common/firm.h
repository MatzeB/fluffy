/*
 * Project:     libFIRM
 * File name:   ir/common/firm.h
 * Purpose:     Central firm header.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file firm.h
 *
 *  Central FIRM header.
 *
 *  FIRM is a full graph based intermediate representation in SSA Form
 *  with a novel concept to model side effects.  It allows fast, aggressive
 *  optimizations.
 *
 *  This header is the central header of the library implementation of this
 *  IR.
 *
 *  The internal representation of a program in firm is separated into five
 *  different modules:
 *   - Firm Graphs representing the code of a program. (Subdirectory ir.)
 *     Firm Graphs are assembled out of several data structures:
 *     irprog: represents a program.  Allows access to all types and all
 *       FIRM graphs for procedures and other global things.
 *     irgraph: represents a procedure.  Allows access to the code of the
 *       procedure, the actual FIRM graph.
 *     irnode: A node of a FIRM graph.  Nodes are typed with an opcode and a mode
 *   and represent instructions in a program.
 *     irop: The opcode of FIRM nodes.
 *     irmode: The mode of FIRM nodes.  Most modes correspond to machine known
 *       data types (int, float, pointer).
 *   - Entities representing program known objects. (Subdirectory tr.)
 *     All variables and procedures are entities.
 *   - Types describing the type system for the program. (Subdirectory tr.)
 *   - Target Values representing program known constants. (Subdirectory tv.)
 *   - Identifiers representing any Strings used in the program. (Subdirectory ident.)
 *
 *   Further this library supplies functionality to build and optimize FIRM graphs
 *   and further functionality needed in a compiler.  Finally there is more
 *   generic functionality to support implementations using firm.  (Code generation,
 *   further optimizations).
 */
#ifndef _FIRM_H_
#define _FIRM_H_

#ifdef __cplusplus
extern "C" {
#endif

/* The representations */
#include "firm_common.h"   /* common type tags. */
#include "irprog.h"        /* control flow and data of a program */
#include "irgraph.h"       /* graphs */
#include "type.h"          /* type representation */
#include "entity.h"        /* entity representation */
#include "tv.h"            /* target values */
#include "ident.h"         /* source code identificators */

/* Functionality */
#include "ircons.h"        /* construct ir */
#include "ircgcons.h"      /* construct interprocedural graph */

/* Optimizations */
#include "irflag.h"         /* optimization flags */
#include "irgopt.h"         /* optimize ir */
#include "reassoc.h"        /* optimize ir by reassociation */
#include "ldstopt.h"        /* optimize Load/Store */
#include "cfopt.h"          /* optimize control flow */
#include "tailrec.h"        /* optimize tail-recursion calls */
#include "ircgopt.h"        /* Optimizations based on interprocedural graph */
#include "opt_osr.h"        /* Operator Strength Reduction */
#include "strength_red.h"   /* Old (and buggy) Strength reduction */
#include "loop_unrolling.h" /* Do loop unrolling */
#include "ifconv.h"         /* if conversion */
#include "funccall.h"       /* real function call optimization */
#include "return.h"         /* Return node normalizations */
#include "scalar_replace.h" /* Scalar replacement */
#include "proc_cloning.h"   /* procedure cloning */
#include "opt_confirms.h"   /* Confirm based optimizations */
#include "gvn_pre.h"        /* global value numbering and partial redundancy elimination */
#include "opt_frame.h"      /* frame type optimization */
#include "tropt.h"          /* optimize the type representation */
#include "condeval.h"       /* control flow optimization by conditional evaluation */

/* Lowering */
#include "lower_calls.h"      /* lowering of different calls */
#include "lower_intrinsics.h" /* lowering of intrinsic calls */
#include "lower_dw.h"         /* double word types lowering */

/* Analyses */
#include "irouts.h"           /* Graph reversal / out edges. */
#include "trouts.h"           /* Graph reversal / out edges for types. */
#include "irdom.h"            /* Dominator analysis */
#include "cgana.h"            /* Analysis to construct interprocedural graph */
                              /* including some optimizations */
#include "irloop.h"           /* loop and backedge analysis */
#include "callgraph.h"        /* Callgraph construction */
#include "irconsconfirm.h"    /* Confirm nodes */
#include "analyze_irg_args.h" /* Simple pointer parameter analysis */
#include "irtypeinfo.h"       /* type information for nodes */
#include "irmemory.h"         /* memory disambiguation */
#include "interval_analysis.h"
#include "field_temperature.h" 
#include "execution_frequency.h"

/* Support */
#include "irgmod.h"         /* Support to modify ir */
#include "irgwalk.h"        /* Support to walk ir */
#include "typewalk.h"       /* Support to walk type information */
#include "typegmod.h"       /* Support to modify type graph */
#include "type_identify.h"  /* Support for type identification */
#include "mangle.h"         /* Support for mangling ident names. */
#include "tr_inheritance.h" /* Support to handle inheritance. */

#include "irarch.h"        /* architecture dependent optimizations */
#include "archop.h"        /* architecture dependent opcodes */
//#include "modeconv.h"      /* architecture dependent mode conversion */

#include "firmstat.h"      /* statistics */

#include "irreflect.h"     /* reflection */

#include "seqnumbers.h"    /* debug support */
#include "firm_ycomp.h"    /* ycomp debugging support */

  
/* @@@ temporarily for jni builder until preprocessor works.
   Then it should be sufficient to include <file.h> instead
   of firm.h as not all enums are needed in the implementation
   files. */
#include "irdump.h"
#include "irprintf.h"
#include "irvrfy.h"
#include "trvrfy.h"

#include "irarch.h"

#include "iredges.h"

/* Macros that define the old function names we decided to rename. 
   Use for compatibility with old implementations. */
/*#include "old_fctnames.h"*/

/**
 * libFirm initialization parameters.
 */
struct _firm_parameter_t {
  /**
   * The size of this structure. init_firm() will only initialize
   * this amount of data. This allows to add more fields to this structure
   * without breaking compatibility to older source.
   */
  unsigned int size;

  /**
   * Statistic options. If statistic function where enabled, these
   * flags configure it, see enum firmstat_options_t.
   */
  unsigned enable_statistics;

  /**
   * This function is called, whenever a local variable is 
   * used before definition. The function should insert a default value,
   * and/or raise a compiler error/warning. Note that returning
   * an Unknown is allowed here.
   */
  uninitialized_local_variable_func_t *initialize_local_func; 

  /**
   * The interface functions for the type identification module.
   * If not set, the default implementation with compare_strict() and
   * firm_hash_name() will be used.
   */
  type_identify_if_t *ti_if;

  /**
   * The interface for the ident module.
   * If not set, the default libFirm ident module (using hash sets).
   */
  ident_if_t *id_if;

  /**
   * The architecture dependent opcode settings.
   * If not set, no architecture dependent operations will be used.
   */
  const arch_ops_info *arch_op_settings;

  /**
   * The default calling convention.
   */
  unsigned cc_mask;

  /**
   * The debug info that should be used for "builtin" objects.
   */
  dbg_info *builtin_dbg;
};

typedef struct _firm_parameter_t firm_parameter_t;

#define libFirm_VERSION_MAJOR 1
#define libFirm_VERSION_MINOR 4

/**
 * The Firm version number.
 */
typedef struct _firm_version_t {
  unsigned major;
  unsigned minor;
} firm_version_t;

/**
 * Initialize the firm library.
 *
 * Initializes the firm library.  Allocates default data structures.
 * Initializes configurable behavior of the library.
 *
 * @param params   A structure containing the parameters of the libFirm. 
 *
 * The parameter struct may be NULL. In that case, the original FIRM behavior 
 * is conserved.
 */
void init_firm(const firm_parameter_t *params);

/**
 * Frees all memory occupied by the firm library.
 */
void free_firm(void);

/**
 * Returns the libFirm version number.
 * If statically linked, always libFirm_VERSION_MAJOR, libFirm_VERSION_MINOR
 */
void firm_get_version(firm_version_t *version);

#ifdef WITH_LIBCORE
/**
 * Read initializations arguments from the .init file.
 */
void firm_init_options(const char *arg_prefix, int argc, const char **argv);
#endif

#ifdef __cplusplus
}
#endif

#endif /* _FIRM_H_ */

