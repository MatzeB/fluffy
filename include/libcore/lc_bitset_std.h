/*
  libcore: library for basic data structures and algorithms.
  Copyright (C) 2005  IPD Goos, Universit"at Karlsruhe, Germany

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/



#include <libcore/lc_bitfiddle.h>

/** Use ordinary ints as unit types. */
typedef unsigned int lc_bitset_unit_t;

#define LC_BITSET_UNIT_FMT "%0x"
#define LC_BITSET_UNIT_ALL_ONE ((unsigned int) -1)

/**
 * Units needed for a given highest bit.
 * This implementation always allocates units in a multiple of 16 bytes.
 * @param size The size of the bitset in bits.
 * @return The number of units needed.
 */
#define _lc_bitset_units(size) (lc_round_up2(size, LC_BS_UNIT_SIZE_BITS) / LC_BS_UNIT_SIZE_BITS)
	
/**
 * Compute the size in bytes needed for a bitseti, overall.
 * This also include the size for the bitset data structure.
 * This implementation computes the size in wat, that the bitset units
 * can be aligned to 16 bytes.
 * @param size The size of the bitset in bits.
 * @return The overall amount of bytes needed for that bitset.
 */
#define _lc_bitset_overall_size(lc_bitset_base_size,size) \
	(lc_bitset_base_size + _lc_bitset_units(size) * LC_BS_UNIT_SIZE)

/**
 * calculate the pointer to the data space of the bitset.
 * @param data The base address of the allocated memory
 * @param lc_bitset_base_size The size of the basical bitset data structure
 * which has to be taken into account.
 * @param size The size of the bitset in bits.
 */
#define _lc_bitset_data_ptr(data,lc_bitset_base_size,size) \
	((lc_bitset_unit_t *) ((char *) data + lc_bitset_base_size))
	

/**
 * Clear some units from a certain address on.
 * @param addr The address from where to clear.
 * @param n The number of units to set to 0.
 */
#define _lc_bitset_inside_clear_units(addr,n) \
	memset(addr, 0, n * LC_BS_UNIT_SIZE)
	
/**
 * Set a bit in a unit.
 * @param unit A pointer to the unit.
 * @param bit which bit to set.
 */
#define _lc_bitset_inside_set(unit_ptr,bit) (*unit_ptr) |= (1 << (bit))

/**
 * Clear a bit in a unit.
 * @param unit A pointer to the unit.
 * @param bit which bit to set.
 */
#define _lc_bitset_inside_clear(unit_ptr,bit) (*unit_ptr) &= ~(1 << (bit))

/**
 * Flip a bit in a unit.
 * @param unit A pointer to the unit.
 * @param bit which bit to set.
 */
#define _lc_bitset_inside_flip(unit_ptr,bit) (*unit_ptr) ^= ~(1 << (bit))

/**
 * Count the number of leading zeroes in a unit.
 * @param unit A pointer to the unit.
 * @return The Number of leading zeroes.
 */
#define _lc_bitset_inside_nlz(unit_ptr) (lc_nlz(*unit_ptr))


/**
 * Count the number of trailing zeroes in a unit.
 * @param unit A pointer to the unit.
 * @return The Number of leading zeroes.
 */
#define _lc_bitset_inside_ntz(unit_ptr) _lc_bitset_std_inside_ntz(unit_ptr)
static LC_INLINE lc_bitset_pos_t _lc_bitset_std_inside_ntz(lc_bitset_unit_t *unit_ptr) 
{
	unsigned long data = *unit_ptr;
	return 32 - (lc_bitset_pos_t) lc_nlz(~data & (data - 1));
}

/**
 * Count the number of trailing zeroes in a unit (whereas the unit is no
 * pointer but a value).
 * @param unit A unit.
 * @return The Number of leading zeroes.
 */
#define _lc_bitset_inside_ntz_value(unit) (32 - lc_nlz(~(unit) & ((unit) - 1))) 

/**
 * test if a bit is set in a unit.
 * @param unit_ptr The pointer to the unit.
 * @param bit The bit to check.
 * @return 1, if the bit is set, 0 otherise.
 */
#define _lc_bitset_inside_is_set(unit_ptr,bit) \
	(((*unit_ptr) & (1 << (bit))) != 0)
	
/**
 * count the number of bits set in a unit.
 * @param unit_ptr The pointer to a unit.
 * @return The number of bits set in the unit.
 */
#define _lc_bitset_inside_pop(unit_ptr) (lc_popcnt(*unit_ptr))

#define _LC_BITSET_BINOP_UNITS_INC 1

#define _lc_bitset_inside_binop_and(tgt,src) ((*tgt) &= (*src))
#define _lc_bitset_inside_binop_andnot(tgt,src) ((*tgt) &= ~(*src))
#define _lc_bitset_inside_binop_or(tgt,src) ((*tgt) |= (*src))
#define _lc_bitset_inside_binop_xor(tgt,src) ((*tgt) ^= (*src))

