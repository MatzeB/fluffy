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
 * @file bitfiddle.h
 * @date 28.9.2004
 * @brief Functions from hackers delight.
 * 
 * Attention! These functions silently assume, that an int is 32 bit wide.
 * $Id$
 */

#ifndef _LC_BITFIDDLE_H
#define _LC_BITFIDDLE_H

#include <libcore/lc_config.h>

#define LC_HACKDEL_WORDSIZE 32

/**
 * Compute the count of set bits in a 32-bit word.
 * @param x A 32-bit word.
 * @return The number of bits set in x.
 */
static LC_INLINE unsigned lc_popcnt(unsigned x) {
  x = x - ((x >> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0f0f0f0f;
  x = x + (x >> 8);
  x = x + (x >> 16);
  return x & 0x3f;
}

/**
 * Compute the number of leading zeroes in a word.
 * @param x The word.
 * @return The number of leading (from the most significant bit) zeroes.
 */
static LC_INLINE unsigned lc_nlz(unsigned x) {
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return lc_popcnt(~x);
}

/**
 * Compute the number of trailing zeroes in a word.
 * @param x The word.
 * @return The number of trailing zeroes.
 */
#define lc_ntz(x) (LC_HACKDEL_WORDSIZE - lc_nlz(~(x) & ((x) - 1)))

/**
 * Compute the greatest power of 2 smaller or equal to a value.
 * This is also known as the binary logarithm.
 * @param x The value.
 * @return The power of two.
 */
#define lc_log2_floor(x) (LC_HACKDEL_WORDSIZE - 1 - lc_nlz(x))

/**
 * Compute the smallest power of 2 greater or equal to a value.
 * This is also known as the binary logarithm.
 * @param x The value.
 * @return The power of two.
 */
#define lc_log2_ceil(x) (LC_HACKDEL_WORDSIZE - lc_nlz((x) - 1))

/**
 * Round up to the next multiple of a power of two.
 * @param x A value.
 * @param pot A power of two.
 * @return x rounded up to the next multiple of pot.
 */
#define lc_round_up2(x,pot) (((x) + ((pot) - 1)) & (~((pot) - 1)))


#endif
