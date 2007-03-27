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


/**
 * @file bitset.h
 * @date 15.10.2004
 * @author Sebastian Hack
 * @brief A bitset implementation.
 */

#ifndef _LC_BITSET_H
#define _LC_BITSET_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include <libcore/lc_config.h>
#include <libcore/lc_bitfiddle.h>

typedef unsigned int lc_bitset_pos_t;

#include <libcore/lc_bitset_std.h>

/*
#if defined(__GNUC__) && defined(__i386__) 
#include "lc_bitset_ia32.h"
#endif
*/

typedef struct _lc_bitset_t {
	lc_bitset_pos_t units;
	lc_bitset_pos_t size;
	lc_bitset_unit_t *data;
} lc_bitset_t;

#define LC_BS_UNIT_SIZE       sizeof(lc_bitset_unit_t)
#define LC_BS_UNIT_SIZE_BITS  (LC_BS_UNIT_SIZE * 8)
#define LC_BS_UNIT_MASK       (LC_BS_UNIT_SIZE_BITS - 1)

/**
 * Initialize a bitset.
 * This functions should not be called.
 *
 * Note that this function needs three macros which must be provided by the
 * bitfield implementor:
 * - _lc_bitset_overall_size(size) The overall size that must be
 *   allocated for the bitfield in bytes.
 * - _lc_bitset_units(size) The number of units that will be
 *   present in the bitfield for a given highest bit.
 * - _lc_bitset_data_ptr(data, size) This produces as pointer to the
 *   first unit in the allocated memory area. The main reason for this
 *   macro is, that some bitset implementors want control over memory
 *   alignment.
 * 
 * @param area A pointer to memory reserved for the bitset.
 * @param size The size of the bitset in bits.
 * @return A pointer to the initialized bitset.
 */
static LC_INLINE lc_bitset_t *_lc_bitset_prepare(void *area, lc_bitset_pos_t size) 
{
	lc_bitset_t *ptr = area;
	memset(area, 0, _lc_bitset_overall_size(sizeof(lc_bitset_t), size));
	ptr->units = _lc_bitset_units(size);
  ptr->size = size; 
	ptr->data = _lc_bitset_data_ptr(area, sizeof(lc_bitset_t), size);
	return ptr;
}

/**
 * Mask out all bits, which are only there, because the number
 * of bits in the set didn't match a unit size boundary.
 * @param bs The bitset.
 * @return The masked bitset.
 */
static LC_INLINE lc_bitset_t *_lc_bitset_mask_highest(lc_bitset_t *bs)
{
	lc_bitset_pos_t rest        = bs->size & LC_BS_UNIT_MASK;
	if(rest)
		bs->data[bs->units - 1] &= (1 << rest) - 1;
  return bs;
}

/**
 * Get the capacity of the bitset in bits.
 * @param bs The bitset.
 * @return The capacity in bits of the bitset. 
 */
#define lc_bitset_capacity(bs) ((bs)->units * LC_BS_UNIT_SIZE_BITS)

/**
 * Get the size of the bitset in bits.
 * @note Note the difference between capacity and size.
 * @param bs The bitset.
 * @return The highest bit which can be set or cleared plus 1.
 */
#define lc_bitset_size(bs)  ((bs)->size)
	
/**
 * Allocate a bitset on an obstack.
 * @param obst The obstack.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define lc_bitset_obstack_alloc(obst,size) \
  _lc_bitset_prepare(obstack_alloc(obst, _lc_bitset_overall_size(sizeof(lc_bitset_t), size)), size)

/**
 * Allocate a bitset via malloc.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define lc_bitset_malloc(size) \
	_lc_bitset_prepare(malloc(_lc_bitset_overall_size(sizeof(lc_bitset_t), size)), size)

/**
 * Free a bitset allocated with lc_bitset_malloc().
 * @param bs The bitset.
 */
#define lc_bitset_free(bs) free(bs)

/**
 * Allocate a bitset on the stack via alloca.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define lc_bitset_alloca(size) \
	_lc_bitset_prepare(alloca(_lc_bitset_overall_size(sizeof(lc_bitset_t), size)), size)


/**
 * Get the unit which contains a specific bit.
 * This function is internal.
 * @param bs The bitset.
 * @param bit The bit.
 * @return A pointer to the unit containing the bit.
 */
static LC_INLINE lc_bitset_unit_t *_lc_bitset_get_unit(const lc_bitset_t *bs, lc_bitset_pos_t bit)
{
	/* assert(bit < bs->units * LC_BS_UNIT_SIZE_BITS && "Bit too large"); */
  assert(bit <= bs->size && "Bit to large");
	return bs->data + bit / LC_BS_UNIT_SIZE_BITS;
}

/**
 * Set a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to set.
 */
static LC_INLINE void lc_bitset_set(lc_bitset_t *bs, lc_bitset_pos_t bit) 
{ 
	lc_bitset_unit_t *unit = _lc_bitset_get_unit(bs, bit); 
	assert(bit < bs->size);
	_lc_bitset_inside_set(unit, bit & LC_BS_UNIT_MASK); 
} 

/**
 * Clear a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to clear.
 */
static LC_INLINE void lc_bitset_clear(lc_bitset_t *bs, lc_bitset_pos_t bit) 
{ 
	lc_bitset_unit_t *unit = _lc_bitset_get_unit(bs, bit); 
	assert(bit < bs->size);
	_lc_bitset_inside_clear(unit, bit & LC_BS_UNIT_MASK); 
} 

/**
 * Check, if a bit is set.
 * @param bs The bitset.
 * @param bit The bit to check for.
 * @return 1, if the bit was set, 0 if not.
 */
static LC_INLINE int lc_bitset_is_set(const lc_bitset_t *bs, lc_bitset_pos_t bit)
{
	lc_bitset_unit_t *unit = _lc_bitset_get_unit(bs, bit);
	assert(bit < bs->size);
	return _lc_bitset_inside_is_set(unit, bit & LC_BS_UNIT_MASK);
}

/**
 * Flip a bit in a bitset.
 * @param bs The bitset.
 * @param bit The bit to flip.
 */
static LC_INLINE void lc_bitset_flip(lc_bitset_t *bs, lc_bitset_pos_t bit) 
{ 
	lc_bitset_unit_t *unit = _lc_bitset_get_unit(bs, bit); 
	assert(bit < bs->size);
	_lc_bitset_inside_flip(unit, bit & LC_BS_UNIT_MASK); 
}

/**
 * Copy a bitset to another.
 * @param tgt The target bitset.
 * @param src The source bitset.
 * @return The target bitset.
 */
static LC_INLINE lc_bitset_t *lc_bitset_copy(lc_bitset_t *tgt, const lc_bitset_t *src) 
{
	lc_bitset_pos_t tu = tgt->units;
	lc_bitset_pos_t su = src->units;
	lc_bitset_pos_t min_units = tu < su ? tu : su;
	memcpy(tgt->data, src->data, min_units * LC_BS_UNIT_SIZE);
	if(tu > min_units)
		memset(tgt->data + min_units, 0, LC_BS_UNIT_SIZE * (tu - min_units));
	return _lc_bitset_mask_highest(tgt);
}
	
/** 
 * Find the smallest bit set in the bitset.
 * @param bs The bitset.
 * @return The smallest bit set in the bitset.
 */
static LC_INLINE lc_bitset_pos_t lc_bitset_min(const lc_bitset_t *bs) 
{ 
	lc_bitset_pos_t i, ofs = 0;

	for(i = 0; i < bs->units; ++i) {
		lc_bitset_unit_t *unit = &bs->data[i];
		lc_bitset_pos_t pos = _lc_bitset_inside_ntz(unit);
		if(pos > 0)
			return ofs + pos;
		ofs += LC_BS_UNIT_SIZE_BITS;
	}

	return 0;
} 

/** 
 * Find the greatest bit set in the bitset.
 * @param bs The bitset.
 * @return The greatest bit set in the bitset.
 */
static LC_INLINE lc_bitset_pos_t lc_bitset_max(const lc_bitset_t *bs)
{ 
	lc_bitset_pos_t i, max = 0, ofs = 0;

	for(i = 0; i < bs->units; ++i) {
		lc_bitset_unit_t *unit = &bs->data[i];
		lc_bitset_pos_t pos = _lc_bitset_inside_nlz(unit);
		if(pos > 0)
			max = ofs + pos;
		ofs += LC_BS_UNIT_SIZE_BITS;
	}

	return max;
}

/**
 * Find the next set bit from a given bit.
 * @note Note that if pos is set, pos is returned. 
 * @param bs The bitset.
 * @param pos The bit from which to search for the next set bit.
 * @return The next set bit from pos on, or -1, if no set bit was found
 * after pos.
 */
static LC_INLINE lc_bitset_pos_t _lc_bitset_next(const lc_bitset_t *bs, 
		lc_bitset_pos_t pos, int set)
{
	lc_bitset_pos_t unit_number = pos / LC_BS_UNIT_SIZE_BITS;
	lc_bitset_pos_t res;

	if(pos >= bs->size)
		return -1;

	{
		lc_bitset_pos_t bit_in_unit = pos & LC_BS_UNIT_MASK;
		lc_bitset_pos_t in_unit_mask = (1 << bit_in_unit) - 1;

		/* 
		 * Mask out the bits smaller than pos in the current unit.
		 * We are only interested in bits set higher than pos.
		 */
		lc_bitset_unit_t curr_unit = bs->data[unit_number];

		/* 
		 * Find the next bit set in the unit. 
		 * Mind that this function returns 0, if the unit is -1 and 
		 * counts the bits from 1 on.
		 */ 
		lc_bitset_pos_t next_in_this_unit =
			_lc_bitset_inside_ntz_value((set ? curr_unit : ~curr_unit) & ~in_unit_mask);

		/* If there is a bit set in the current unit, exit. */
		if (next_in_this_unit < LC_BS_UNIT_SIZE_BITS) {
			res = next_in_this_unit + unit_number * LC_BS_UNIT_SIZE_BITS;
			return res < bs->size ? res : -1;
		}

		/* Else search for set bits in the next units. */
		else {
			lc_bitset_pos_t i;
			for(i = unit_number + 1; i < bs->units; ++i) {
				lc_bitset_unit_t data = bs->data[i];
				lc_bitset_pos_t first_set = 
					_lc_bitset_inside_ntz_value(set ? data : ~data);

				if (first_set < LC_BS_UNIT_SIZE_BITS) {
					res = first_set + i * LC_BS_UNIT_SIZE_BITS;
					return res < bs->size ? res : -1;
				}
			}
		}
	}

	return -1;
}

#define lc_bitset_next_clear(bs,pos) _lc_bitset_next((bs), (pos), 0)
#define lc_bitset_next_set(bs,pos) _lc_bitset_next((bs), (pos), 1)

/**
 * Convenience macro for bitset iteration.
 * @param bitset The bitset.
 * @param elm A unsigned long variable.
 */
#define lc_bitset_foreach(bitset,elm) \
  for(elm = lc_bitset_next_set(bitset,0); elm != -1; elm = lc_bitset_next_set(bitset,elm+1))


#define lc_bitset_foreach_clear(bitset,elm) \
  for(elm = lc_bitset_next_clear(bitset,0); elm != -1; elm = lc_bitset_next_clear(bitset,elm+1))

/**
 * Count the bits set.
 * This can also be seen as the cardinality of the set.
 * @param bs The bitset.
 * @return The number of bits set in the bitset.
 */
static LC_INLINE lc_bitset_pos_t lc_bitset_popcnt(const lc_bitset_t *bs) 
{
	lc_bitset_pos_t i, pop = 0;
	lc_bitset_unit_t *unit;

	for(i = 0, unit = bs->data; i < bs->units; ++i, ++unit)
		pop += _lc_bitset_inside_pop(unit);

	return pop;
}

/**
 * Clear the bitset.
 * This sets all bits to zero.
 * @param bs The bitset.
 */
static LC_INLINE lc_bitset_t *lc_bitset_clear_all(lc_bitset_t *bs) 
{
	memset(bs->data, 0, LC_BS_UNIT_SIZE * bs->units);
  return bs;
}

/**
 * Set the bitset.
 * This sets all bits to one.
 * @param bs The bitset.
 */
static LC_INLINE lc_bitset_t *lc_bitset_set_all(lc_bitset_t *bs) 
{
	memset(bs->data, -1, LC_BS_UNIT_SIZE * bs->units);
	return _lc_bitset_mask_highest(bs);
}

/**
 * Check, if one bitset is contained by another. 
 * That is, each bit set in lhs is also set in rhs.
 * @param lhs A bitset.
 * @param rhs Another bitset.
 * @return 1, if all bits in lhs are also set in rhs, 0 otherwise.
 */
static LC_INLINE int lc_bitset_contains(const lc_bitset_t *lhs, const lc_bitset_t *rhs)
{
	lc_bitset_pos_t n = lhs->units < rhs->units ? lhs->units : rhs->units;
	lc_bitset_pos_t i;

	for(i = 0; i < n; ++i) {
		lc_bitset_unit_t lu = lhs->data[i];
		lc_bitset_unit_t ru = rhs->data[i];
		
		if((lu | ru) & ~ru)
			return 0;
	}
	
	/*
	 * If the left hand sinde is a larger bitset than rhs, 
	 * we have to check, that all extra bits in lhs are 0
	 */
	if(lhs->units > n) {
		for(i = n; i < lhs->units; ++i) {
			if(lhs->data[i] != 0)
				return 0;
		}
	}

	return 1;
}

/**
 * Check, if a bitset is all zero.
 * @param bs The bitset.
 * @return 1, if the bitset is all zero, 0 if not.
 */
static LC_INLINE int lc_bitset_is_all_zero(const lc_bitset_t *bs)
{
  lc_bitset_pos_t i;

  for(i = 0; i < bs->units; ++i) {
    lc_bitset_unit_t unit = bs->data[i];
		if(unit != 0)
			return 0;
	}

	return 1;
}

/**
 * Treat the bitset as a number and subtract 1.
 * @param bs The bitset.
 * @return The same (modified) bitset.
 */
static LC_INLINE void lc_bitset_minus1(lc_bitset_t *bs)
{
  lc_bitset_pos_t i;

  for(i = 0; i < bs->units; ++i) {
    lc_bitset_unit_t unit = bs->data[i];
    lc_bitset_unit_t um1  = unit - 1;

    bs->data[i] = um1;

    if(((unit >> LC_BS_UNIT_MASK) ^ (um1 >> LC_BS_UNIT_MASK)) == 0)
      break;
  }
}

/**
 * Print a bitset to a stream.
 * The bitset is printed as a comma seperated list of bits set.
 * @param file The stream.
 * @param bs The bitset.
 */ 
static LC_INLINE void lc_bitset_fprint(FILE *file, const lc_bitset_t *bs) 
{
	const char *prefix = "";
	int i;
	
	putc('[', file);
	for(i = lc_bitset_next_set(bs, 0); i != -1; i = lc_bitset_next_set(bs, i + 1)) {
		fprintf(file, "%s%u", prefix, i);
		prefix = ",";
	}
	putc(']', file);
}

static LC_INLINE void lc_bitset_debug_fprint(FILE *file, const lc_bitset_t *bs)
{
	lc_bitset_pos_t i;

	fprintf(file, "%u:", bs->units);
	for(i = 0; i < bs->units; ++i)
		fprintf(file, " " LC_BITSET_UNIT_FMT, bs->data[i]); 
}

/*
 * Here, the binary operations follow.
 * And, Or, And Not, Xor are available.
 */
#define _LC_BINARY_OP(op) \
static LC_INLINE lc_bitset_t *lc_bitset_ ## op(lc_bitset_t *tgt, const lc_bitset_t *src) \
{ \
	lc_bitset_pos_t i; \
	lc_bitset_pos_t n = tgt->units > src->units ? src->units : tgt->units; \
	for(i = 0; i < n; i += _LC_BITSET_BINOP_UNITS_INC) \
		_lc_bitset_inside_binop_ ## op(&tgt->data[i], &src->data[i]); \
	if(n < tgt->units) \
		_lc_bitset_clear_rest(&tgt->data[i], tgt->units - i); \
	return _lc_bitset_mask_highest(tgt); \
}

/* 
 * Define the clear rest macro for the and, since it is the only case,
 * were non existed (treated as 0) units in the src must be handled.
 * For all other operations holds: x Op 0 = x for Op in { Andnot, Or, Xor }
 *
 * For and, each bitset implementer has to provide the macro
 * _lc_bitset_clear_units(data, n), which clears n units from the pointer
 * data on.
 */
#define _lc_bitset_clear_rest(data,n) _lc_bitset_inside_clear_units(data, n)
_LC_BINARY_OP(and)
#undef _lc_bitset_clear_rest
#define _lc_bitset_clear_rest(data,n) (void) 0

_LC_BINARY_OP(andnot) 
_LC_BINARY_OP(or)     
_LC_BINARY_OP(xor)    

#endif
