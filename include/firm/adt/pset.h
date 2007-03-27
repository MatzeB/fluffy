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

#ifndef _PSET_H
#define _PSET_H

#include <stddef.h>

#include "hashptr.h"
#include "iterator.h"

/**
 * The default comparison function for pointers.
 * @param x A pointer.
 * @param y A pointer.
 * @return 0 if @p x and @p y are equal. Some value != 0 otherwise.
 */
int pset_default_ptr_cmp(const void *x, const void *y); 

/** 
 * The abstract type of a pset (Set of pointers). 
 *
 * This kind of sets stores only pointer to elements, the elements itself
 * must be stored somewhere else.
 *
 * @see set
 */
typedef struct pset pset;

/* 
 * Define some convenience macros using the predefined hash function.
 */
#define pset_insert_ptr(set,key)  pset_insert(set, key, HASH_PTR(key))
#define pset_hinsert_ptr(set,key) pset_hinsert(set, key, HASH_PTR(key))
#define pset_remove_ptr(set,key)  pset_remove(set, key, HASH_PTR(key))
#define pset_find_ptr(set,key)    pset_find(set, key, HASH_PTR(key))
#define pset_new_ptr(slots)       new_pset(pset_default_ptr_cmp, slots)
#define pset_new_ptr_default()    pset_new_ptr(64)

/** The entry of a pset, representing an element pointer in the set and it's meta-information */
typedef struct {
  unsigned hash;
  void *dptr;
} pset_entry;

/**
 * The type of a set compare function.
 *
 * @param elt   pointer to an element
 * @param key   pointer to another element
 *
 * @return
 *    0 if the elements are identically, non-zero else
 */
typedef int (*pset_cmp_fun) (const void *elt, const void *key);

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
pset *new_pset (pset_cmp_fun func, int slots);

/**
 * Deletes a pset.
 *
 * @param pset   the pset
 *
 * @note
 *    This does NOT delete the elements of this pset, just it's pointers!
 */
void del_pset (pset *pset);

/**
 * Returns the number of elements in a pset.
 *
 * @param pset   the pset
 */
int pset_count (pset *pset);

/**
 * Searches an element pointer in a pset.
 *
 * @param pset  the pset to search in
 * @param key   the element to search
 * @param hash  the hash value of key
 *
 * @return
 *    the pointer of the found element in the pset or NULL if it was not found
 */
void *pset_find (pset *pset, const void *key, unsigned hash);

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
 *    It is not possible to insert an element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its already existing set_entry.

 */
void *pset_insert (pset *pset, const void *key, unsigned hash);

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
 *    It is not possible to insert an element more than once. If an element
 *    that should be inserted is already in the pset, this functions does
 *    nothing but returning its pset_entry.
 */
pset_entry *pset_hinsert (pset *pset, const void *key, unsigned hash);

/**
 * Removes an element from a pset.
 * 
 * @param pset  the pset to delete in
 * @param key   a pointer to the element to be deleted
 * @param hash  the hash-value of the element
 *
 * @return
 *    the pointer to the removed element
 *
 * @remark
 *    The current implementation did not allow to remove non-existing elements.
 *    @@@ so, does it do now?
 *    Further, it is allowed to remove elements during an iteration
 *    including the current one.
 */
void *pset_remove (pset *pset, const void *key, unsigned hash);

/**
 * Returns the first element of a pset.
 * 
 * @param pset  the pset to iterate
 *
 * @return a pointer to the element or NULL if the set is empty
 */
void *pset_first (pset *pset);

/**
 * Returns the next element of a pset.
 *
 * @param pset  the pset to iterate
 *
 * @return a pointer to the next element or NULL if the
 *         iteration is finished
 */
void *pset_next (pset *pset);

/**
 * Breaks the iteration of a set. Must be called before
 * the next pset_first() call if the iteration was NOT
 * finished.
 *
 * @param pset  the pset
 */
void pset_break (pset *pset);

/**
 * Iterates oven an pset.
 *
 * @param pset   the pset
 * @param entry  the iterator
 */
#define foreach_pset(pset, entry) for (entry = pset_first(pset); entry; entry = pset_next(pset))

/** 
 * Inserts all elements of the pointer set src into
 * the set target (union). 
 *
 * @param target  the target set, will contain the union
 * @param src     a set, will not be changed
 */
void pset_insert_pset_ptr(pset *target, pset *src);

#define new_pset(cmp, slots) (PSET_TRACE (new_pset) ((cmp), (slots)))
#define pset_find(pset, key, hash) \
  _pset_search ((pset), (key), (hash), _pset_find)
#define pset_insert(pset, key, hash) \
  _pset_search ((pset), (key), (hash), _pset_insert)
#define pset_hinsert(pset, key, hash) \
  ((pset_entry *)_pset_search ((pset), (key), (hash), _pset_hinsert))

#ifdef STATS
/**
 * Prints statistics on a set to stdout.
 *
 * @param pset  the pset
 */
void pset_stats (pset *pset);
#else
# define pset_stats(s) ((void)0)
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
void pset_describe (pset *pset);
#endif

/* @@@ NYI */
#define PSET_VRFY(pset) (void)0


/* Private */

typedef enum { _pset_find, _pset_insert, _pset_hinsert } _pset_action;

void *_pset_search (pset *, const void *, unsigned, _pset_action);

#if defined(DEBUG) && defined(HAVE_GNU_MALLOC)
extern const char *pset_tag;
# ifdef PSET_ID
#   define PSET_TRACE pset_tag = SET_ID,
# else
#   define PSET_TRACE pset_tag = __FILE__,
# endif
#else /* !(DEBUG && HAVE_GNU_MALLOC) */
#   define PSET_TRACE
#endif /* !(DEBUG && HAVE_GNU_MALLOC) */

#endif
