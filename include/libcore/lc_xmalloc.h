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
 * xmalloc/xfree
 * @author Sebastian Hack
 * @date 3.1.2005
 */

#ifndef _LIBCORE_XMALLOC_H
#define _LIBCORE_XMALLOC_H

#include <stddef.h>

extern void *lc_xmalloc(size_t size);
extern void *lc_xcalloc(size_t memb, size_t size);
extern void *lc_xrealloc(void *ptr, size_t size);
extern void lc_xfree(void *ptr);

#endif /* _LIBCORE_XMALLOC_H */
