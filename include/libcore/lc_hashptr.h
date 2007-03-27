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


/*
 * Hash function for pointers.
 * @author  Michael Beck, Sebastian Hack
 * @date 22.12.2004
 */

#ifndef _LC_HASHPTR_H
#define _LC_HASHPTR_H

#include <stddef.h>
#include <libcore/lc_config.h>

#define _LC_FNV_OFFSET_BASIS 2166136261U
#define _LC_FNV_PRIME 16777619U

/* Computing x * _FIRM_FNV_FNV_PRIME */
#define _LC_FNV_TIMES_PRIME(x) \
  (((x) << 24) + ((x) << 8) + ((x) << 7) + ((x) << 4) + ((x) << 1) + 1)

/**
 * Init value for hashing.
 */
#define LC_HASH_INIT	_LC_FNV_OFFSET_BASIS

/**
 * Iterative hashing.
 * @param hash The hash value computed up to now (The inital hash should
 *             be set to LC_HASH_INIT.
 * @param buf  The data to hash.
 * @param len  The number of bytes to hash.
 * @return The new hash value.
 */
static LC_INLINE unsigned lc_fnv_hash_add(unsigned hash, const void *buf, size_t len)
{
	const unsigned char *data = buf;
	size_t i;

	for(i = 0; i < len; ++i) {
		hash = _LC_FNV_TIMES_PRIME(hash);
		hash ^= data[i];
	}

	return hash;
}

#define LC_HASH_ADD_VAR(h,x)     lc_fnv_hash_add((h), &(x), sizeof(x))
#define LC_HASH_ADD_STR(h,s,len) lc_fnv_hash_add((h), (s), (len))

#define LC_FNV_HASH(buf, len) lc_fnv_hash_add(LC_HASH_INIT, (buf), (len))

/**
 * hash a pointer value: Pointer addresses are mostly aligned to 4
 * or 8 bytes. So we remove the lowest 3 bits
 */
#define LC_HASH_PTR(ptr)    (((char *) (ptr) - (char *)0) >> 3)

/**
 * Hash a string.
 * @param str The string (can be const).
 * @param len The length of the string.
 * @return A hash value for the string.
 */
#define LC_HASH_STR(str,len) LC_FNV_HASH(str, len)

#endif /* _HASHPTR_H */

