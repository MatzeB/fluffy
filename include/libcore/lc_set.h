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
 * Project:     libFIRM
 * File name:   ir/adt/set.h
 * Purpose:     Declarations for set.
 * Author:      Markus Armbruster 
 * Modified by: 
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster 
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file set.h
 *
 * Declarations for set.
 */

#ifndef _LC_SET_H
#define _LC_SET_H

#include <stddef.h>

/**
 * The abstract type of a set. 
 *
 * This sets stores copies of its elements, so there is no need
 * to store the elements after they were added to a set.
 *
 * @see pset
 */
typedef struct lc_set lc_set;

/** The entry of a set, representing an element in the set and it's meta-information */
typedef struct lc_set_entry {
  unsigned hash;    /**< the hash value of the element */
  size_t size;      /**< the size of the element */
  int dptr[1];	    /**< the element itself, data copied in must not need more
		          alignment than this */
} lc_set_entry;

/**
 * The type of a set compare function.
 *
 * @param elt   pointer to an element
 * @param key   pointer to another element
 * @param size  size of the elements
 *
 * @return
 *    0 if the elements are identically, non-zero else
 *
 * @note
 *    Although it is possible to define different meanings of equality
 *    of two elements of a set, they can be only equal if their sizes are
 *    are equal. This is checked before the compare function is called.
 */
typedef int (*lc_set_cmp_fun) (const void *elt, const void *key, size_t size);

/**
 * Creates a new set.
 *
 * @param func    The compare function of this set.
 * @param slots   Initial number of collision chains.  I.e., #slots 
 *                different keys can be hashed without collisions. 
 *
 * @returns
 *    created set
 */
lc_set *lc_set_new (lc_set_cmp_fun func, int slots);

/**
 * Deletes a set and all elements of it.
 */
void lc_set_del (lc_set *set);

/**
 * Returns the number of elements in a set.
 *
 * @param set   the set
 */
int lc_set_count (lc_set *set);

/**
 * Searches an element in a set.
 *
 * @param set   the set to search in
 * @param key   the element to is searched
 * @param size  the size of key
 * @param hash  the hash value of key
 *
 * @return
 *    The address of the found element in the set or NULL if it was not found.
 */
void *lc_set_find (lc_set *set, const void *key, size_t size, unsigned hash);

/**
 * Inserts an element into a set.  
 * 
 * @param set   the set to insert in
 * @param key   a pointer to the element to be inserted.  Element is copied!
 * @param size  the size of the element that should be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the inserted element
 *
 * @note
 *    It is not possible to insert on element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its pointer.
 */
void *lc_set_insert (lc_set *set, const void *key, size_t size, unsigned hash);

/**
 * Inserts an element into a set and returns its set_entry.
 * 
 * @param set   the set to insert in
 * @param key   a pointer to the element to be inserted. Element is copied!
 * @param size  the size of the element that should be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the set_entry of the inserted element
 *
 * @note
 *    It is not possible to insert an element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its set_entry.
 */
lc_set_entry *lc_set_hinsert (lc_set *set, const void *key, size_t size, unsigned hash);

/**
 * Inserts an element into a set, zero-terminate it and returns its set_entry.
 * 
 * @param set   the set to insert in
 * @param key   a pointer to the element to be inserted.  Element is copied!
 * @param size  the size of the element that should be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the set_entry of the inserted element
 *
 * @note
 *    It is not possible to insert on element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its set_entry.
 */
lc_set_entry *lc_set_hinsert0 (lc_set *set, const void *key, size_t size, unsigned hash);

/**
 * Returns the first element of a set.
 * 
 * @param set  the set to iterate
 *
 * @return a pointer to the element or NULL if the set is empty
 */
void *lc_set_first (lc_set *set);

/**
 * Returns the next element of a set.
 *
 * @param set  the set to iterate
 *
 * @return a pointer to the next element or NULL if the
 *         iteration is finished
 */
void *lc_set_next (lc_set *set);

/**
 * Breaks the iteration of a set. Must be called before
 * the next pset_first() call if the iteration was NOT
 * finished.
 *
 * @param pset  the pset
 */
void lc_set_break (lc_set *set);

#if defined(DEBUG) && defined(HAVE_GNU_MALLOC)
extern const char *lc_set_tag;
# ifdef LC_SET_ID
#   define LC_SET_TRACE lc_set_tag = LC_SET_ID,
# else
#   define LC_SET_TRACE lc_set_tag = __FILE__,
# endif
#else /* !(DEBUG && HAVE_GNU_MALLOC) */
#   define LC_SET_TRACE
#endif /* !(DEBUG && HAVE_GNU_MALLOC) */

/* implementation specific */
#define lc_new_set(cmp, slots) (LC_SET_TRACE (new_set) ((cmp), (slots)))
#define lc_set_find(set, key, size, hash) \
  _lc_set_search ((set), (key), (size), (hash), _lc_set_find)
#define lc_set_insert(set, key, size, hash) \
  _lc_set_search ((set), (key), (size), (hash), _lc_set_insert)
#define lc_set_hinsert(set, key, size, hash) \
  ((lc_set_entry *)_lc_set_search ((set), (key), (size), (hash), _lc_set_hinsert))
#define lc_set_hinsert0(set, key, size, hash) \
  ((lc_set_entry *)_lc_set_search ((set), (key), (size), (hash), _lc_set_hinsert0))

#define LC_SET_VRFY(set) (void)0

#ifdef LC_STATS
/**
 * Prints statistics on a set to stdout.
 *
 * @param set  the set
 */
void lc_set_stats (set *set);
#else
# define lc_set_stats(s) ((void)0)
#endif

#ifdef LC_DEBUG
/**
 * Describe a set.
 *
 * Writes a description of a set to stdout. The description includes:
 * - a header telling how many elements (nkey) and segments (nseg) are in use
 * - for every collision chain the number of element with its hash values
 *
 * @param set  the set
 */
void lc_set_describe (set *set);
#endif


/* Private */

typedef enum { 
  _lc_set_find, 
  _lc_set_insert, 
  _lc_set_hinsert, 
  _lc_set_hinsert0 
} _lc_set_action;

void *_lc_set_search (lc_set *, const void *, size_t, unsigned, _lc_set_action);

#endif

