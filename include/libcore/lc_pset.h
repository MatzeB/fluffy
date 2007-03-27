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
 * File name:   ir/adt/pset.h
 * Purpose:     Declarations for pset.
 * Author:      Markus Armbruster 
 * Modified by: 
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster 
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _LC_PSET_H
#define _LC_PSET_H

#include <stddef.h>

#include <libcore/lc_hashptr.h>

/**
 * The default comparison function for pointers.
 * @param x A pointer.
 * @param y A pointer.
 * @return 0 if @p x and @p y are equal. Some value != 0 otherwise.
 */
int lc_pset_default_ptr_cmp(const void *x, const void *y); 

/* 
 * Define some convenience macros using the predefined hash function.
 */
#define lc_pset_insert_ptr(set,key)     lc_pset_insert(set, key, LC_HASH_PTR(key))
#define lc_pset_hinsert_ptr(set,key)    lc_pset_hinsert(set, key, LC_HASH_PTR(key))
#define lc_pset_remove_ptr(set,key)     lc_pset_remove(set, key, LC_HASH_PTR(key))
#define lc_pset_find_ptr(set,key)       lc_pset_find(set, key, LC_HASH_PTR(key))
#define lc_pset_new_ptr(slots)          lc_new_pset(lc_pset_default_ptr_cmp, slots)
#define lc_pset_new_ptr_default()       lc_pset_new_ptr(64)

/** 
 * The abstract type of a pset (Set of pointers). 
 *
 * This kind of sets stores only pointer to elements, the elements itself
 * must be stored somewere else.
 *
 * @see set
 */
typedef struct lc_pset lc_pset;

/** The entry of a pset, representing an element pointer in the set and it's meta-information */
typedef struct {
  unsigned hash;
  void *dptr;
} lc_pset_entry;

/**
 * The type of a set compare function.
 *
 * @param elt   pointer to an element
 * @param key   pointer to another element
 *
 * @return
 *    0 if the elements are identically, non-zero else
 */
typedef int (*lc_pset_cmp_fun) (const void *elt, const void *key);

/**
 * Creates a new pset.
 *
 * @param func    The compare function of this pset.
 * @param slots   Initial number of collision chains.  I.e., #slots
 *                different keys can be hashed without collisions.
 *
 * @returns
 *    created pset
 */
lc_pset *lc_pset_new(lc_pset_cmp_fun func, int slots);

/**
 * Deletes a pset.
 *
 * @param pset   the pset
 *
 * @note
 *    This does NOT delete the elements of this pset, just it's pointers!
 */
void lc_pset_del (lc_pset *pset);

/**
 * Returns the number of elements in a pset.
 *
 * @param pset   the pset
 */
int lc_pset_count (lc_pset *pset);

/**
 * Searches an element pointer in a pset.
 *
 * @param pset  the pset to search in
 * @param key   the element to search
 * @param hash  the hash value of key
 *
 * @return
 *    the pointer of the found element in the pset of NULL if it was not found
 */
void *lc_pset_find (lc_pset *pset, const void *key, unsigned hash);

/**
 * Inserts an element pointer into a pset.
 * 
 * @param pset  the pset to insert in
 * @param key   a pointer to the element to be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the inserted element
 *
 * @note
 *    It is not possible to insert on element more than once. If a element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its already existing set_entry.

 */
void *lc_pset_insert (lc_pset *pset, const void *key, unsigned hash);

/**
 * Inserts an element pointer into a pset and returns its pset_entry.
 * 
 * @param pset  the pset to insert in
 * @param key   a pointer to the element to be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the pset_entry of the inserted element
 *
 * @note
 *    It is not possible to insert on element more than once. If a element
 *    that should be inserted is already in the pset, this functions does
 *    nothing but returning its pset_entry.
 */
lc_pset_entry *lc_pset_hinsert (lc_pset *pset, const void *key, unsigned hash);

/**
 * Removes an element from a pset.
 * 
 * @param pset  the pset to insert in
 * @param key   a pointer to the element to be inserted
 * @param hash  the hash-value of the element
 *
 * @return
 *    the pointer to the removed element
 *
 * @remark
 *    The current implementation did not allow to remove non-existing elements.
 *    Further, it is allowed to remove elements during an iteration
 *    including the current one.
 */
void *lc_pset_remove (lc_pset *pset, const void *key, unsigned hash);

/**
 * Returns the first element of a pset.
 * 
 * @param pset  the pset to iterate
 *
 * @return a pointer to the element or NULL if the set is empty
 */
void *lc_pset_first (lc_pset *pset);

/**
 * Returns the next element of a pset.
 *
 * @param pset  the pset to iterate
 *
 * @return a pointer to the next element or NULL if the
 *         iteration is finished
 */
void *lc_pset_next (lc_pset *pset);

/**
 * Breaks the iteration of a set. Must be called before
 * the next pset_first() call if the iteration was NOT
 * finished.
 *
 * @param pset  the pset
 */
void lc_pset_break (lc_pset *pset);

/** 
 * Inserts all elements of the pointer set src into
 * the set target (union). 
 *
 * @param target  the target set, will contain the union
 * @param src     a set, will not be changed
 */
void lc_pset_insert_pset_ptr(lc_pset *target, lc_pset *src);

#if defined(LC_DEBUG) && defined(HAVE_GNU_MALLOC)
extern const char *lc_pset_tag;
# ifdef LC_PSET_ID
#   define LC_PSET_TRACE lc_pset_tag = LC_SET_ID,
# else
#   define LC_PSET_TRACE lc_pset_tag = __FILE__,
# endif
#else /* !(DEBUG && HAVE_GNU_MALLOC) */
#   define LC_PSET_TRACE
#endif /* !(DEBUG && HAVE_GNU_MALLOC) */

#define lc_pset_new(cmp, slots) (LC_PSET_TRACE (lc_pset_new) ((cmp), (slots)))
#define lc_pset_find(pset, key, hash) \
  _lc_pset_search ((pset), (key), (hash), _lc_pset_find)
#define lc_pset_insert(pset, key, hash) \
  _lc_pset_search ((pset), (key), (hash), _lc_pset_insert)
#define lc_pset_hinsert(pset, key, hash) \
  ((lc_pset_entry *)_lc_pset_search ((pset), (key), (hash), _lc_pset_hinsert))

#ifdef LC_STATS
/**
 * Prints statistics on a set to stdout.
 *
 * @param pset  the pset
 */
void lc_pset_stats (lc_pset *pset);
#else
# define lc_pset_stats(s) ((void)0)
#endif

#ifdef DEBUG
/**
 * Describe a pset.
 *
 * Writes a description of a set to stdout. The description includes:
 * - a header telling how many elements (nkey) and segments (nseg) are in use
 * - for every collision chain the number of element with its hash values
 *
 * @param pset  the pset
 */
void lc_pset_describe (lc_pset *pset);
#endif

/* @@@ NYI */
#define PSET_VRFY(pset) (void)0


/* Private */

typedef enum { _lc_pset_find, _lc_pset_insert, _lc_pset_hinsert } _lc_pset_action; 

void *_lc_pset_search (lc_pset *, const void *, unsigned, _lc_pset_action);

#endif
