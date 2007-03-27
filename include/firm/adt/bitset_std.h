#ifndef _BITSET_STD_H
#define _BITSET_STD_H

#include "bitfiddle.h"

/** Use ordinary ints as unit types. */
typedef unsigned int bitset_unit_t;

#define BITSET_UNIT_FMT "%0x"
#define BITSET_UNIT_ALL_ONE ((unsigned int) -1)

/**
 * Units needed for a given highest bit.
 * This implementation always allocates units in a multiple of 16 bytes.
 * @param size The size of the bitset in bits.
 * @return The number of units needed.
 */
#define _bitset_units(size) (round_up2(size, BS_UNIT_SIZE_BITS) / BS_UNIT_SIZE_BITS)
	
/**
 * Compute the size in bytes needed for a bitseti, overall.
 * This also include the size for the bitset data structure.
 * This implementation computes the size in wat, that the bitset units
 * can be aligned to 16 bytes.
 * @param size The size of the bitset in bits.
 * @return The overall amount of bytes needed for that bitset.
 */
#define _bitset_overall_size(bitset_base_size,size) \
	(bitset_base_size + _bitset_units(size) * BS_UNIT_SIZE)

/**
 * calculate the pointer to the data space of the bitset.
 * @param data The base address of the allocated memory
 * @param bitset_base_size The size of the basical bitset data structure
 * which has to be taken into account.
 * @param size The size of the bitset in bits.
 */
#define _bitset_data_ptr(data,bitset_base_size,size) \
	((bitset_unit_t *) ((char *) data + bitset_base_size))
	

/**
 * Clear some units from a certain address on.
 * @param addr The address from where to clear.
 * @param n The number of units to set to 0.
 */
#define _bitset_inside_clear_units(addr,n) \
	memset(addr, 0, n * BS_UNIT_SIZE)
	
/**
 * Set a bit in a unit.
 * @param unit A pointer to the unit.
 * @param bit which bit to set.
 */
#define _bitset_inside_set(unit_ptr,bit) (*unit_ptr) |= (1 << (bit))

/**
 * Clear a bit in a unit.
 * @param unit A pointer to the unit.
 * @param bit which bit to set.
 */
#define _bitset_inside_clear(unit_ptr,bit) (*unit_ptr) &= ~(1 << (bit))

/**
 * Flip a bit in a unit.
 * @param unit A pointer to the unit.
 * @param bit which bit to set.
 */
#define _bitset_inside_flip(unit_ptr,bit) (*unit_ptr) ^= (1 << (bit))

/**
 * Flip a whole unit.
 * @param unit_ptr The pointer to the unit.
 */
#define _bitset_inside_flip_unit(unit_ptr) (*unit_ptr) = ~(*unit_ptr)

/**
 * Count the number of leading zeroes in a unit.
 * @param unit A pointer to the unit.
 * @return The Number of leading zeroes.
 */
#define _bitset_inside_nlz(unit_ptr) (nlz(*unit_ptr))


/**
 * Count the number of trailing zeroes in a unit.
 * @param unit A pointer to the unit.
 * @return The Number of leading zeroes.
 */
#define _bitset_inside_ntz(unit_ptr) _bitset_std_inside_ntz(unit_ptr)
static INLINE unsigned _bitset_std_inside_ntz(bitset_unit_t *unit_ptr) 
{
	unsigned long data = *unit_ptr;
	return 32 - (unsigned) nlz(~data & (data - 1));
}

/**
 * Count the number of trailing zeroes in a unit (whereas the unit is no
 * pointer but a value).
 * @param unit A unit.
 * @return The Number of leading zeroes.
 */
#define _bitset_inside_ntz_value(unit) (32 - nlz(~(unit) & ((unit) - 1))) 

/**
 * test if a bit is set in a unit.
 * @param unit_ptr The pointer to the unit.
 * @param bit The bit to check.
 * @return 1, if the bit is set, 0 otherise.
 */
#define _bitset_inside_is_set(unit_ptr,bit) \
	(((*unit_ptr) & (1 << (bit))) != 0)
	
/**
 * count the number of bits set in a unit.
 * @param unit_ptr The pointer to a unit.
 * @return The number of bits set in the unit.
 */
#define _bitset_inside_pop(unit_ptr) (popcnt(*unit_ptr))

#define _BITSET_BINOP_UNITS_INC 1

#define _bitset_inside_binop_and(tgt,src) ((*tgt) &= (*src))
#define _bitset_inside_binop_andnot(tgt,src) ((*tgt) &= ~(*src))
#define _bitset_inside_binop_or(tgt,src) ((*tgt) |= (*src))
#define _bitset_inside_binop_xor(tgt,src) ((*tgt) ^= (*src))

#endif
