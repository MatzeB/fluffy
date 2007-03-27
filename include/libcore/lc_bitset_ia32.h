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



#ifndef _LC_BITSET_IA32_H
#define _LC_BITSET_IA32_H

#include <libcore/lc_bitfiddle.h>

#undef _lc_bitset_inside_clear
#undef _lc_bitset_inside_set
#undef _lc_bitset_inside_flip
#undef _lc_bitset_inside_is_set

#undef _lc_bitset_inside_nlz
#undef _lc_bitset_inside_ntz
#undef _lc_bitset_inside_ntz_value

#define _lc_bitset_inside_set(unit,bit) \
	__asm__( "btsl %1,%0" :"=m" (unit) :"Ir" (bit))

#define _lc_bitset_inside_clear(unit,bit) \
	__asm__( "btrl %1,%0" :"=m" (unit) :"Ir" (bit))

#define _lc_bitset_inside_flip(unit,bit) \
	__asm__( "btcl %1,%0" :"=m" (unit) :"Ir" (bit))
	
#define _lc_bitset_inside_is_set(unit,bit) _lc_bitset_ia32_inside_is_set(unit, bit)
#define _lc_bitset_inside_nlz(unit) _lc_bitset_ia32_inside_nlz(unit)
#define _lc_bitset_inside_ntz(unit) _lc_bitset_ia32_inside_ntz(unit)
#define _lc_bitset_inside_ntz_value(unit) _lc_bitset_ia32_inside_ntz_value(unit)

static INLINE int _lc_bitset_ia32_inside_is_set(unsigned long *unit, unsigned bit)
{
	int res = 0;
	__asm__("mov $0,%0\n\tbtl %1,%2\n\tadc $0,%0"
			: "=r" (res) 
			: "Ir" (bit), "m" (unit) 
			: "cc");
	return res;
}

static INLINE unsigned _lc_bitset_ia32_inside_nlz(unsigned long *unit)
{
	unsigned res = 0;
	__asm__("bsrl %1,%0" :"=r" (res) :"m" (unit));
	return *unit == 0 ? 32 : res;
}

static INLINE unsigned _lc_bitset_ia32_inside_ntz(unsigned long *unit) {
	unsigned res = 0;
	__asm__("bsfl %1,%0" :"=r" (res) :"m" (unit));
	return *unit == 0 ? 32 : res;
}

static INLINE unsigned _lc_bitset_ia32_inside_ntz_value(unsigned long unit) {
	unsigned res = 0;
	__asm__("bsfl %1,%0" :"=r" (res) :"rm" (unit));
	return unit == 0 ? 32 : res;
}

#if defined(__GNUC__) && defined(__SSE2__)

#include <stddef.h>
#include <xmmintrin.h>

#undef _lc_bitset_units 
#undef _lc_bitset_overall_size 
#undef _lc_bitset_data_ptr 

#undef _LC_BITSET_BINOP_UNITS_INC 

#undef _lc_bitset_inside_binop_and
#undef _lc_bitset_inside_binop_andnot
#undef _lc_bitset_inside_binop_or
#undef _lc_bitset_inside_binop_xor

#undef _lc_bitset_inside_binop_with_zero_and
#undef _lc_bitset_inside_binop_with_zero_andnot
#undef _lc_bitset_inside_binop_with_zero_or
#undef _lc_bitset_inside_binop_with_zero_xor

#define _lc_bitset_units(highest_bit) (lc_round_up2(highest_bit, 128) / LC_BS_UNIT_SIZE_BITS)
	
#define _lc_bitset_overall_size(lc_bitset_base_size,highest_bit) \
	((lc_bitset_base_size) + 16 + _lc_bitset_units(highest_bit) * LC_BS_UNIT_SIZE)

#define _lc_bitset_data_ptr(data,lc_bitset_base_size,highest_bit) \
  _lc_bitset_sse_data_ptr(data, lc_bitset_base_size, highest_bit)

static INLINE unsigned long *_lc_bitset_sse_data_ptr(void *data, size_t lc_bitset_base_size, 
		unsigned long highest_bit)
{
	ptrdiff_t diff;
	char *units = data;

	diff = (units - (char *) 0) + lc_bitset_base_size;
	diff = lc_round_up2(diff, 16);
	units = (char *) 0 + diff;
	return (unsigned long *) units;
}

#define _LC_BITSET_BINOP_UNITS_INC 4
#define _lc_bitset_inside_binop_and(tgt,src) _lc_bitset_sse_inside_binop_and(tgt,src)
#define _lc_bitset_inside_binop_andnot(tgt,src) _lc_bitset_sse_inside_binop_andnot(tgt,src)
#define _lc_bitset_inside_binop_or(tgt,src) _lc_bitset_sse_inside_binop_or(tgt,src)
#define _lc_bitset_inside_binop_xor(tgt,src) _lc_bitset_sse_inside_binop_xor(tgt,src)

#define _LC_BITSET_SSE_BINOP(name) \
static INLINE void _lc_bitset_sse_inside_binop_ ## name(unsigned long *tgt, unsigned long *src) \
{ \
	__m128i src_op = _mm_load_si128((__m128i *) src); \
	__m128i tgt_op = _mm_load_si128((__m128i *) tgt); \
	__m128i res = _mm_ ## name ## _si128(tgt_op, src_op); \
	_mm_store_si128((void *) tgt, res); \
} 


static INLINE void _lc_bitset_sse_inside_binop_with_zero_and(unsigned long *tgt) 
{
	tgt[0] = 0;
	tgt[1] = 0;
	tgt[2] = 0;
	tgt[3] = 0;
}

static INLINE void _lc_bitset_sse_inside_binop_andnot(unsigned long *tgt, unsigned long *src) 
{ 
	__m128i src_op = _mm_load_si128((void *) src);
	__m128i tgt_op = _mm_load_si128((void *) tgt);
	__m128i res = _mm_andnot_si128(src_op, tgt_op); 
	_mm_store_si128((__m128i *) tgt, res); 
}

_LC_BITSET_SSE_BINOP(and)
_LC_BITSET_SSE_BINOP(or)
_LC_BITSET_SSE_BINOP(xor)


#endif
#endif
