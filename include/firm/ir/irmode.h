/*
 * Project:     libFIRM
 * File name:   ir/ir/irmode.h
 * Purpose:     Data modes of operations.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Mathias Heil, Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2007 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irmode.h
 *    irmode -- Modes for ir operators
 *
 * @author Christian Schaefer, Matthias Heil
 *
 * This module specifies the modes that type the firm nodes.  It defines
 * a datasturcture that describes a mode and implements constructors and
 * access routines to this datastructure. Further it defines a set of
 * predefined modes.
 *
 * SEE ALSO:
 *    UKA tech report 1999-44 for more information about modes.
 *
 */

#ifndef _FIRM_IR_IRMODE_H_
#define _FIRM_IR_IRMODE_H_

#include "firm_types.h"
#include "ident.h"

/**
 * Contains relevant information about a mode.
 *
 * Necessary information about a mode is stored in this struct
 * which is used by the tarval module to perform calculations
 * and comparisons of values of a such described mode.
 *
 * ATTRIBUTES:
 *  -  modecode code:           An unambiguous int (enum) for the mode
 *  -  ident *name:             Name of this mode. Two modes are different if the name is different.
 *  -  mode_sort sort:          sort of mode specifying possible usage categories
 *  -  int    size:             size of the mode in Bits.
 *  -  unsigned sign:1:         signedness of this mode
 *  -  ... more to come
 *  -  modulo_shift             specifies for modes of kind irms_int_number
 *                              whether shift applies modulo to value of bits to shift
 *
 * SEE ALSO:
 *    The tech report 1999-44 describing FIRM and predefined modes
 *    tarval.h
 */
#ifndef _IR_MODE_TYPEDEF_
#define _IR_MODE_TYPEDEF_
typedef struct ir_mode ir_mode;
#endif

/* ********** Predefined modes ********** */

/**
 * Predefined mode according to tech report 1999-14.
 */
typedef enum { /* irm is short for `ir mode' */
	irm_BB,                       /**< basic block */
	irm_X,                        /**< execution */
	irm_F,                        /**< float(32) */
	irm_D,                        /**< double(64) */
	irm_E,                        /**< extended(80) */
	irm_Bs,                       /**< signed byte(8) */
	irm_Bu,                       /**< unsigned byte(8) */
	irm_Hs,                       /**< signed short(16) */
	irm_Hu,                       /**< unsigned short(16) */
	irm_Is,                       /**< signed int(32) */
	irm_Iu,                       /**< unsigned int(32) */
	irm_Ls,                       /**< signed long(64) */
	irm_Lu,                       /**< unsigned long(64) */
	irm_LLs,                      /**< signed long long(128) */
	irm_LLu,                      /**< unsigned long long(128) */
	irm_C,                        /**< character */
	irm_P,                        /**< pointer */
	irm_b,                        /**< internal boolean */
	irm_M,                        /**< memory */
	irm_T,                        /**< tuple */
	irm_U,                        /**< unicode character */
	irm_ANY,                      /**< undefined mode */
	irm_BAD,                      /**< bad mode */
	irm_max                       /**< maximum value for modecode */
} modecode;

/** These values represent the different mode classes of value representations.
 */
typedef enum {
	/* Predefined sorts of modes */
	irms_auxiliary,         /**< Only for Firm use. Not extensible. (irm_T) */
	irms_control_flow,      /**< Marks all control flow modes. Not extensible. (irm_BB, irm_X) */
	irms_memory,            /**< Marks the memory mode.  Not extensible. (irm_M) */
	irms_internal_boolean,  /**< Internal boolean representation.
	                             Storing to memory impossible, convert first. (irm_b) */
	/* user-extensible sorts of modes */
	irms_int_number,        /**< A mode to represent int numbers.
	                             Integer computations can be performed. */
	irms_float_number,      /**< A mode to represent float numbers.
	                             Floating point computations can be performed. */
	irms_reference,         /**< A mode to represent entities.
	                             Restricted int computations can be performed */
	irms_character          /**< A mode to represent characters/symbols
	                             ?? Are computations allowed? as int?? */
} mode_sort;

/** These values represent the different arithmetic operations possible with a mode.
    Further arithmetics can be defined, e.g., for @@@ modes.
 */
typedef enum {
	irma_uninitialized = 0,
	irma_none = 1,              /**< For modes for which no representation is specified.
	                                 These are modes of sort auxiliary, internal_boolean and character. */
	irma_twos_complement = 2,   /**< Values of the mode are represented as two's complement.
                                     Only legal for modes of sort int_number and reference. */
	irma_ones_complement,       /**< Values of the mode are represented  as one's complement.
	                                 Only legal for modes of sort int_number and reference. */
	irma_int_BCD,               /**< Values of the mode are represented as binary coded decimals.
	                                 Only legal for modes of sort int_number and reference. */
	irma_ieee754 = 256,         /**< Values of the mode are represented according to ieee754
                                     floatingpoint standard.  Only legal for modes of sort float_number. */
	irma_float_BCD,             /**< Values of the mode are represented  as binary coded decimals
	                                 according to @@@ which standards??? Only legal for modes of
	                                 sort float_number. */
	irma_max
} mode_arithmetic;


/* ********** Constructor for user defined modes **************** */
/**
 * Creates a new mode.
 *
 * @param name          the name of the mode to be created
 * @param sort          the mode_sort of the mode to be created
 * @param bit_size      number of bits this mode allocate
 * @param sign          non-zero if this is a signed mode
 * @param arithmetic    arithmetic operations possible with a mode
 * @param modulo_shift  Is ignored for modes other than integer.
 *
 * This function constructs a new mode given by the parameters.
 * If the parameters match an already defined mode, this mode is returned
 * (including the default modes).
 * If the mode is newly allocated, a new unique mode_code is chosen.
 * Also, special value tarvals will be calculated such as null,
 * min, max and can be retrieved using the get_mode_* functions
 *
 * @return
 *   The new mode or NULL on error.
 *
 * @note
 *   It is allowed to construct the default modes. So, a call
 *   new_ir_mode("Is", irms_int_number, 32, 1, irma_twos_complement, 32) will return mode_Is.
 */
ir_mode *new_ir_mode(const char *name, mode_sort sort, int bit_size, int sign, mode_arithmetic arithmetic, unsigned int modulo_shift);

/**
 * Creates a new vector mode.
 *
 * @param name          the name of the mode to be created
 * @param sort          the mode_sort of the mode to be created
 * @param bit_size      number of bits for one element of this mode
 * @param num_of_elem   number of elements in this vector mode
 * @param sign          non-zero if this is a signed mode
 * @param arithmetic    arithmetic operations possible with a mode
 * @param modulo_shift  Is ignored for modes other than integer.
 *
 * This function constructs a new vector mode given by the parameters.
 * If the parameters match an already defined mode, this mode is returned.
 * If the mode is newly allocated, a new unique mode_code is chosen.
 * Also, special value tarvals will be calculated such as null,
 * min, max and can be retrieved using the get_mode_* functions
 *
 * @return
 *   The new mode or NULL on error.
 */
ir_mode *new_ir_vector_mode(const char *name, mode_sort sort, int bit_size, unsigned num_of_elem, int sign,
                            mode_arithmetic arithmetic, unsigned int modulo_shift);

/**
 * Checks whether a pointer points to a mode.
 *
 * @param thing     an arbitrary pointer
 *
 * @return
 *     true if the thing is a mode, else false
 */
int is_mode(void *thing);

/* ********** Access methods to read mode information *********** */

/** Returns the classification of the mode */
modecode    get_mode_modecode(const ir_mode *mode);

/** Returns the ident* of the mode */
ident      *get_mode_ident(const ir_mode *mode);

/** Returns the null-terminated name of this mode. */
const char *get_mode_name(const ir_mode *mode);

/** Returns a coarse classification of the mode. */
mode_sort   get_mode_sort(const ir_mode *mode);

/** Returns the size of values of the mode in bits. */
int get_mode_size_bits(const ir_mode *mode);

/** Returns the size of values of the mode in bytes.
 *  If the size is not dividable by 8 returns -1. */
int get_mode_size_bytes(const ir_mode *mode);

/** Returns the signess of a mode.
 *
 * Returns the signess of a mode: 1 if mode is signed. */
int get_mode_sign(const ir_mode *mode);

/** Returns the arithmetic of a mode */
int get_mode_arithmetic(const ir_mode *mode);

/** Get the modulo shift attribute.
 *
 *  Attribute modulo shift specifies for modes of kind irms_int_number
 *  whether shift applies modulo to value of bits to shift.  Zero for
 *  modes that are not integer.
 */
unsigned int get_mode_modulo_shift(const ir_mode *mode);

/** Return the number of vector elements.
 *
 *  Attribute vector_elem specifies the number of vector elements of
 *  a vector mode. For non-vector modes it returns 1 for data and 0
 *  for all other modes
 */
unsigned int get_mode_n_vector_elems(const ir_mode *mode);

/** Returns the stored intermediate information. */
void *get_mode_link(const ir_mode *mode);

/** Stores new intermediate information. */
void  set_mode_link(ir_mode *mode, void *l);

/**
 * Returns the smallest representable value of a given mode.
 *
 * For modes of the sort float_number this is the most negative value
 * bigger than -infinite.
 */
tarval *get_mode_min(ir_mode *mode);

/**
 * Returns the biggest representable value o f a given mode.
 *
 * For modes of the sort float_number this is the largest value lower
 * than infinite.
 */
tarval *get_mode_max(ir_mode *mode);

/**
 * Returns the value Zero represented in this mode.
 *
 * Zero is the additive neutral element and as such
 * is defined only for modes allowing addition, i.e.
 * op_pin_state_floats and ints, and references (NULL-Pointer)
 * else returns tarval_bad.
 */
tarval *get_mode_null(ir_mode *mode);

/**
 * Returns the value One, represented in this mode.
 *
 * One, being the multiplicative neutral element,
 * is defined only for modes allowing multiplication,
 * i.e. ints and floats.
 */
tarval *get_mode_one(ir_mode *mode);

/**
 * Returns the value Minus One, represented in this mode.
 *
 * Minus One is defined only for modes allowing
 * multiplication with signed values, i.e. signed ints and floats.
 */
tarval *get_mode_minus_one(ir_mode *mode);

/**
 * Returns the positive infinite value of a mode.
 *
 * This is only valid for float_numbers, other modes
 * will result in tarval_bad.
 */
tarval *get_mode_infinite(ir_mode *mode);

/**
 * Returns the NAN value of a given mode.
 *
 * This is only valid for float_numbers, other modes
 * will result in tarval_bad.
 */
tarval *get_mode_NAN(ir_mode *mode);

extern ir_mode *mode_M;	 /**< memory */

/* -- A set of predefined, numerical modes according to Techreport 1999-44 -- */
extern ir_mode *mode_F;	  /**< signed float(32) */
extern ir_mode *mode_D;   /**< signed double(64) */
extern ir_mode *mode_E;   /**< signed extended(80) */
extern ir_mode *mode_Bs;  /**< signed byte (former char) */
extern ir_mode *mode_Bu;  /**< unsigned byte (former char) */
extern ir_mode *mode_Hs;  /**< signed short integer */
extern ir_mode *mode_Hu;  /**< unsigned short integer */
extern ir_mode *mode_Is;  /**< signed integer */
extern ir_mode *mode_Iu;  /**< unsigned integer */
extern ir_mode *mode_Ls;  /**< signed long integer */
extern ir_mode *mode_Lu;  /**< unsigned long integer */
extern ir_mode *mode_LLs; /**< signed long long integer */
extern ir_mode *mode_LLu; /**< unsigned long long integer */

extern ir_mode *mode_C;   /**< 8 bit char */
extern ir_mode *mode_U;   /**< 16 bit unicode char */

extern ir_mode *mode_P;   /**< pointer */
extern ir_mode *mode_P_code; /**< A pointer mode that is set by the client of libfirm.  This mode
                                  represents the pointer size of the target machine code addresses. Is initialized
                                  to mode_P. */
extern ir_mode *mode_P_data; /**< A pointer mode that is set by the client of libfirm.  This mode
                                  represents the pointer size of the target machine data addresses. Is initialized
                                  to mode_P. */

/* -- Auxiliary modes necessary for the Firm representation -- */
extern ir_mode *mode_b;  /**< internal boolean */

extern ir_mode *mode_X;  /**< execution */
extern ir_mode *mode_BB; /**< block */

extern ir_mode *mode_T;  /**< tuple (none) */
extern ir_mode *mode_ANY;/**< undefined mode */
extern ir_mode *mode_BAD;/**< bad mode */

/*@{*/
/** Access routines for JNI Interface */
ir_mode *get_modeF(void);
ir_mode *get_modeD(void);
ir_mode *get_modeE(void);
ir_mode *get_modeBs(void);
ir_mode *get_modeBu(void);
ir_mode *get_modeHs(void);
ir_mode *get_modeHu(void);
ir_mode *get_modeIs(void);
ir_mode *get_modeIu(void);
ir_mode *get_modeLs(void);
ir_mode *get_modeLu(void);
ir_mode *get_modeLLs(void);
ir_mode *get_modeLLu(void);
ir_mode *get_modeC(void);
ir_mode *get_modeU(void);
ir_mode *get_modeP(void);
ir_mode *get_modeb(void);
ir_mode *get_modeX(void);
ir_mode *get_modeBB(void);
ir_mode *get_modeM(void);
ir_mode *get_modeT(void);
ir_mode *get_modeANY(void);
ir_mode *get_modeBAD(void);

/** Returns the machine specific pointer mode for code addresses. */
ir_mode *get_modeP_code(void);

/** Returns the machine specific pointer mode for data addresses. */
ir_mode *get_modeP_data(void);

/**
 * Sets the machine specific pointer mode for code addresses.
 * If not set, the predefined mode mode_P will be used.
 */
void set_modeP_code(ir_mode *p);

/**
 * Sets the machine specific pointer mode for data addresses.
 * If not set, the predefined mode mode_P will be used.
 */
void set_modeP_data(ir_mode *p);

/**
   Functions to check, whether a modecode is signed, float, int, character,
   reference, num, numP, data, datab or dataM.

   For more exact definitions read the corresponding pages
   in the firm documentation or the following enumeration

   The set of "float" is defined as:
   float = {irm_F, irm_D, irm_E}

   The set of "int" is defined as:
   int   = {irm_Bs, irm_Bu, irm_Hs, irm_Hu, irm_Is, irm_Iu, irm_Ls, irm_Lu}

   The set of "character" is defined as:
   character  = {irm_C, irm_U}

   The set of "reference" is defined as:
   reference  = {irm_P}

   The set of "num" is defined as:
   num   = {float || int}

   The set of "numP" is defined as:
   numP  =  {float || int || reference}

   The set of "data" is defined as:
   data  =  {num || character || reference}

   The set of "datab" is defined as:
   datab =  {data || irm_b }

   The set of "dataM" is defined as:
   dataM =  {data || irm_M}

   Vector "int" and "float" are defined by the arithmetic and vector_elem > 1.
*/
/*@}*/
/* Test for a certain class of modes. */
int mode_is_signed (const ir_mode *mode);
int mode_is_float (const ir_mode *mode);
int mode_is_int (const ir_mode *mode);
int mode_is_character (const ir_mode *mode);
int mode_is_reference (const ir_mode *mode);
int mode_is_num (const ir_mode *mode);
int mode_is_numP (const ir_mode *mode);
int mode_is_data (const ir_mode *mode);
int mode_is_datab (const ir_mode *mode);
int mode_is_dataM (const ir_mode *mode);
int mode_is_float_vector (const ir_mode *mode);
int mode_is_int_vector (const ir_mode *mode);

/** Returns true if sm can be converted to lm without loss
   according to firm definiton */
int smaller_mode(const ir_mode *sm, const ir_mode *lm);

/**
 * Returns a matching unsigned mode for a given integer signed mode.
 * Returns NULL if no matching mode exists.
 */
ir_mode *find_unsigned_mode(const ir_mode *mode);

/**
 * Returns a matching signed mode for a given integer unsigned mode.
 * Returns NULL if no matching mode exists.
 */
ir_mode *find_signed_mode(const ir_mode *mode);

/**
 * Returns an integer mode with 2*n bits for a given integer mode with n bits.
 * Returns NULL if no matching mode exists.
 */
ir_mode *find_double_bits_int_mode(const ir_mode *mode);

/**
 * Returns non-zero if the given mode honors signed zero's, i.e.,
 * a +0 and a -0 exists and handled differently.
 */
int mode_honor_signed_zeros(const ir_mode *mode);

/**
 * Returns non-zero if the given mode might overflow on unary Minus.
 */
int mode_overflow_on_unary_Minus(const ir_mode *mode);

/**
 * Returns non-zero if the mode has a reversed wrap-around
 * logic, especially (a + x) - x == a.
 * This is normally true for integer modes, not for floating
 * point modes.
 */
int mode_wrap_around(const ir_mode *mode);

/**
 * Return the signed integer equivalent mode for an reference mode.
 */
ir_mode *get_reference_mode_signed_eq(ir_mode *mode);

/**
 * Sets the signed integer equivalent mode for an reference mode.
 */
void set_reference_mode_signed_eq(ir_mode *ref_mode, ir_mode *int_mode);

/**
 * Return the unsigned integer equivalent mode for an reference mode.
 */
ir_mode *get_reference_mode_unsigned_eq(ir_mode *mode);

/**
 * Sets the unsigned integer equivalent mode for an reference mode.
 */
void set_reference_mode_unsigned_eq(ir_mode *ref_mode, ir_mode *int_mode);

#endif /* _FIRM_IR_IRMODE_H_ */
