/**
 * @file bitset.h
 * @date 15.10.2004
 * @author Sebastian Hack
 * @brief A bitset implementation.
 */

#ifndef __FIRM_BITSET_H
#define __FIRM_BITSET_H

#include "firm_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif

#include "xmalloc.h"
#include "bitfiddle.h"

typedef unsigned int bitset_pos_t;

#include "bitset_std.h"

/*
#if defined(__GNUC__) && defined(__i386__) 
#include "bitset_ia32.h"
#endif
*/

typedef struct _bitset_t {
	bitset_pos_t units;
	bitset_pos_t size;
	bitset_unit_t *data;
} bitset_t;

#define BS_UNIT_SIZE sizeof(bitset_unit_t)
#define BS_UNIT_SIZE_BITS (BS_UNIT_SIZE * 8)
#define BS_UNIT_MASK (BS_UNIT_SIZE_BITS - 1)

/**
 * Initialize a bitset.
 * This functions should not be called.
 *
 * Note that this function needs three macros which must be provided by the
 * bitfield implementor:
 * - _bitset_overall_size(size) The overall size that must be
 *   allocated for the bitfield in bytes.
 * - _bitset_units(size) The number of units that will be
 *   present in the bitfield for a given highest bit.
 * - _bitset_data_ptr(data, size) This produces as pointer to the
 *   first unit in the allocated memory area. The main reason for this
 *   macro is, that some bitset implementors want control over memory
 *   alignment.
 * 
 * @param area A pointer to memory reserved for the bitset.
 * @param size The size of the bitset in bits.
 * @return A pointer to the initialized bitset.
 */
static INLINE bitset_t *_bitset_prepare(void *area, bitset_pos_t size) 
{
	bitset_t *ptr = area;
	memset(area, 0, _bitset_overall_size(sizeof(bitset_t), size));
	ptr->units = _bitset_units(size);
	ptr->size = size; 
	ptr->data = _bitset_data_ptr(area, sizeof(bitset_t), size);
	return ptr;
}

/**
 * Mask out all bits, which are only there, because the number
 * of bits in the set didn't match a unit size boundary.
 * @param bs The bitset.
 * @return The masked bitset.
 */
static INLINE bitset_t *_bitset_mask_highest(bitset_t *bs)
{
	bitset_pos_t rest = bs->size & BS_UNIT_MASK;
	if (rest)
		bs->data[bs->units - 1] &= (1 << rest) - 1;
	return bs;
}

/**
 * Get the capacity of the bitset in bits.
 * @param bs The bitset.
 * @return The capacity in bits of the bitset. 
 */
#define bitset_capacity(bs) ((bs)->units * BS_UNIT_SIZE_BITS)

/**
 * Get the size of the bitset in bits.
 * @note Note the difference between capacity and size.
 * @param bs The bitset.
 * @return The highest bit which can be set or cleared plus 1.
 */
#define bitset_size(bs)  ((bs)->size)
	
/**
 * Allocate a bitset on an obstack.
 * @param obst The obstack.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_obstack_alloc(obst,size) \
	_bitset_prepare(obstack_alloc(obst, _bitset_overall_size(sizeof(bitset_t), size)), size)

/**
 * Allocate a bitset via malloc.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_malloc(size) \
	_bitset_prepare(xmalloc(_bitset_overall_size(sizeof(bitset_t), size)), size)

/**
 * Free a bitset allocated with bitset_malloc().
 * @param bs The bitset.
 */
#define bitset_free(bs) free(bs)

/**
 * Allocate a bitset on the stack via alloca.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_alloca(size) \
	_bitset_prepare(alloca(_bitset_overall_size(sizeof(bitset_t), size)), size)


/**
 * Get the unit which contains a specific bit.
 * This function is internal.
 * @param bs The bitset.
 * @param bit The bit.
 * @return A pointer to the unit containing the bit.
 */
static INLINE bitset_unit_t *_bitset_get_unit(const bitset_t *bs, bitset_pos_t bit)
{
	/* assert(bit < bs->units * BS_UNIT_SIZE_BITS && "Bit too large"); */
	assert(bit <= bs->size && "Bit to large");
	return bs->data + bit / BS_UNIT_SIZE_BITS;
}

/**
 * Set a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to set.
 */
static INLINE void bitset_set(bitset_t *bs, bitset_pos_t bit) 
{ 
	bitset_unit_t *unit = _bitset_get_unit(bs, bit); 
	_bitset_inside_set(unit, bit & BS_UNIT_MASK); 
} 

/**
 * Clear a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to clear.
 */
static INLINE void bitset_clear(bitset_t *bs, bitset_pos_t bit) 
{ 
	bitset_unit_t *unit = _bitset_get_unit(bs, bit); 
	_bitset_inside_clear(unit, bit & BS_UNIT_MASK); 
} 

/**
 * Check, if a bit is set.
 * @param bs The bitset.
 * @param bit The bit to check for.
 * @return 1, if the bit was set, 0 if not.
 */
static INLINE int bitset_is_set(const bitset_t *bs, bitset_pos_t bit)
{
	bitset_unit_t *unit = _bitset_get_unit(bs, bit);
	return _bitset_inside_is_set(unit, bit & BS_UNIT_MASK);
}

/**
 * Flip a bit in a bitset.
 * @param bs The bitset.
 * @param bit The bit to flip.
 */
static INLINE void bitset_flip(bitset_t *bs, bitset_pos_t bit) 
{ 
	bitset_unit_t *unit = _bitset_get_unit(bs, bit); 
	_bitset_inside_flip(unit, bit & BS_UNIT_MASK); 
}

/**
 * Flip the whole bitset.
 * @param bs The bitset.
 */
static INLINE void bitset_flip_all(bitset_t *bs)
{
	bitset_pos_t i; 
	for(i = 0; i < bs->units; i++)
		_bitset_inside_flip_unit(&bs->data[i]);
	_bitset_mask_highest(bs); 
}

/**
 * Copy a bitset to another.
 * @param tgt The target bitset.
 * @param src The source bitset.
 * @return The target bitset.
 */
static INLINE bitset_t *bitset_copy(bitset_t *tgt, const bitset_t *src) 
{
	bitset_pos_t tu = tgt->units;
	bitset_pos_t su = src->units;
	bitset_pos_t min_units = tu < su ? tu : su;
	memcpy(tgt->data, src->data, min_units * BS_UNIT_SIZE);
	if(tu > min_units)
		memset(tgt->data + min_units, 0, BS_UNIT_SIZE * (tu - min_units));
	return _bitset_mask_highest(tgt);
}
	
/** 
 * Find the smallest bit set in the bitset.
 * @param bs The bitset.
 * @return The smallest bit set in the bitset.
 */
static INLINE bitset_pos_t bitset_min(const bitset_t *bs) 
{ 
	bitset_pos_t i, ofs = 0;

	for(i = 0; i < bs->units; ++i) {
		bitset_unit_t *unit = &bs->data[i];
		bitset_pos_t pos = _bitset_inside_ntz(unit);
		if(pos > 0)
			return ofs + pos;
		ofs += BS_UNIT_SIZE_BITS;
	}

	return 0;
} 

/** 
 * Find the greatest bit set in the bitset.
 * @param bs The bitset.
 * @return The greatest bit set in the bitset.
 */
static INLINE bitset_pos_t bitset_max(const bitset_t *bs)
{ 
	bitset_pos_t i, max = 0, ofs = 0;

	for(i = 0; i < bs->units; ++i) {
		bitset_unit_t *unit = &bs->data[i];
		bitset_pos_t pos = _bitset_inside_nlz(unit);
		if(pos > 0)
			max = ofs + pos;
		ofs += BS_UNIT_SIZE_BITS;
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
static INLINE bitset_pos_t _bitset_next(const bitset_t *bs, 
		bitset_pos_t pos, int set)
{
	bitset_pos_t unit_number = pos / BS_UNIT_SIZE_BITS;
	bitset_pos_t res;

	if(pos >= bs->size)
		return -1;

	{
		bitset_pos_t bit_in_unit = pos & BS_UNIT_MASK;
		bitset_pos_t in_unit_mask = (1 << bit_in_unit) - 1;

		/* 
		 * Mask out the bits smaller than pos in the current unit.
		 * We are only interested in bits set higher than pos.
		 */
		bitset_unit_t curr_unit = bs->data[unit_number];

		/* 
		 * Find the next bit set in the unit. 
		 * Mind that this function returns 0, if the unit is -1 and 
		 * counts the bits from 1 on.
		 */ 
		bitset_pos_t next_in_this_unit =
			_bitset_inside_ntz_value((set ? curr_unit : ~curr_unit) & ~in_unit_mask);

		/* If there is a bit set in the current unit, exit. */
		if (next_in_this_unit < BS_UNIT_SIZE_BITS) {
			res = next_in_this_unit + unit_number * BS_UNIT_SIZE_BITS;
			return res < bs->size ? res : -1;
		}

		/* Else search for set bits in the next units. */
		else {
			bitset_pos_t i;
			for(i = unit_number + 1; i < bs->units; ++i) {
				bitset_unit_t data = bs->data[i];
				bitset_pos_t first_set = 
					_bitset_inside_ntz_value(set ? data : ~data);

				if (first_set < BS_UNIT_SIZE_BITS) {
					res = first_set + i * BS_UNIT_SIZE_BITS;
					return res < bs->size ? res : -1;
				}
			}
		}
	}

	return -1;
}

#define bitset_next_clear(bs,pos) _bitset_next((bs), (pos), 0)
#define bitset_next_set(bs,pos) _bitset_next((bs), (pos), 1)

/**
 * Convenience macro for bitset iteration.
 * @param bitset The bitset.
 * @param elm A unsigned long variable.
 */
#define bitset_foreach(bitset,elm) \
	for(elm = bitset_next_set(bitset,0); elm != -1; elm = bitset_next_set(bitset,elm+1))


#define bitset_foreach_clear(bitset,elm) \
	for(elm = bitset_next_clear(bitset,0); elm != -1; elm = bitset_next_clear(bitset,elm+1))

/**
 * Count the bits set.
 * This can also be seen as the cardinality of the set.
 * @param bs The bitset.
 * @return The number of bits set in the bitset.
 */
static INLINE bitset_pos_t bitset_popcnt(const bitset_t *bs) 
{
	bitset_pos_t i, pop = 0;
	bitset_unit_t *unit;

	for(i = 0, unit = bs->data; i < bs->units; ++i, ++unit)
		pop += _bitset_inside_pop(unit);

	return pop;
}

/**
 * Clear the bitset.
 * This sets all bits to zero.
 * @param bs The bitset.
 */
static INLINE bitset_t *bitset_clear_all(bitset_t *bs) 
{
	memset(bs->data, 0, BS_UNIT_SIZE * bs->units);
	return bs;
}

/**
 * Set the bitset.
 * This sets all bits to one.
 * @param bs The bitset.
 */
static INLINE bitset_t *bitset_set_all(bitset_t *bs) 
{
	memset(bs->data, -1, bs->units * BS_UNIT_SIZE);
	return _bitset_mask_highest(bs);
}

/**
 * Check, if one bitset is contained by another. 
 * That is, each bit set in lhs is also set in rhs.
 * @param lhs A bitset.
 * @param rhs Another bitset.
 * @return 1, if all bits in lhs are also set in rhs, 0 otherwise.
 */
static INLINE int bitset_contains(const bitset_t *lhs, const bitset_t *rhs)
{
	bitset_pos_t n = lhs->units < rhs->units ? lhs->units : rhs->units;
	bitset_pos_t i;

	for(i = 0; i < n; ++i) {
		bitset_unit_t lu = lhs->data[i];
		bitset_unit_t ru = rhs->data[i];
		
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
 * Treat the bitset as a number and subtract 1.
 * @param bs The bitset.
 * @return The same bitset.
 */
static INLINE void bitset_minus1(bitset_t *bs)
{
#define _SH (sizeof(bitset_unit_t) * 8 - 1)
	
	bitset_pos_t i;

	for(i = 0; i < bs->units; ++i) {
		bitset_unit_t unit = bs->data[i];
		bitset_unit_t um1  = unit - 1;

		bs->data[i] = um1;

		if(((unit >> _SH) ^ (um1 >> _SH)) == 0)
			break;
	}
#undef _SH
}

/**
 * Print a bitset to a stream.
 * The bitset is printed as a comma separated list of bits set.
 * @param file The stream.
 * @param bs The bitset.
 */ 
static INLINE void bitset_fprint(FILE *file, const bitset_t *bs) 
{
	const char *prefix = "";
	int i;
	
	putc('{', file);
	for(i = bitset_next_set(bs, 0); i != -1; i = bitset_next_set(bs, i + 1)) {
		fprintf(file, "%s%u", prefix, i);
		prefix = ",";
	}
	putc('}', file);
}

static INLINE void bitset_debug_fprint(FILE *file, const bitset_t *bs)
{
	bitset_pos_t i;

	fprintf(file, "%u:", bs->units);
	for(i = 0; i < bs->units; ++i)
		fprintf(file, " " BITSET_UNIT_FMT, bs->data[i]); 
}

/*
 * Here, the binary operations follow.
 * And, Or, And Not, Xor are available.
 */
#define BINARY_OP(op) \
static INLINE bitset_t *bitset_ ## op(bitset_t *tgt, const bitset_t *src) \
{ \
	bitset_pos_t i; \
	bitset_pos_t n = tgt->units > src->units ? src->units : tgt->units; \
	for(i = 0; i < n; i += _BITSET_BINOP_UNITS_INC) \
		_bitset_inside_binop_ ## op(&tgt->data[i], &src->data[i]); \
	if(n < tgt->units) \
		_bitset_clear_rest(&tgt->data[i], tgt->units - i); \
	return _bitset_mask_highest(tgt); \
}

/* 
 * Define the clear rest macro for the and, since it is the only case,
 * were non existed (treated as 0) units in the src must be handled.
 * For all other operations holds: x Op 0 = x for Op in { Andnot, Or, Xor }
 *
 * For and, each bitset implementer has to provide the macro
 * _bitset_clear_units(data, n), which clears n units from the pointer
 * data on.
 */
#define _bitset_clear_rest(data,n) _bitset_inside_clear_units(data, n)
BINARY_OP(and)
#undef _bitset_clear_rest
#define _bitset_clear_rest(data,n)

BINARY_OP(andnot)
BINARY_OP(or)
BINARY_OP(xor)

#endif
