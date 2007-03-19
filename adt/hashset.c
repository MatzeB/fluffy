/**
 * @file
 * @date    17.03.2007
 * @brief   Geberic hashset implementation
 * @author  Matthias Braun, inspiration from densehash from google sparsehash
 *          package
 * @version $Id$
 */

/* You have to specialize this file by defining:
 * JUMP, ValueType, Hash(x), ValsEqual(x,y), Alloc(size), Free(ptr), EmptyEntry,
 * DeletedEntry, IsEmptyEntry(x), IsDeletedEntry(x), SetRangeEmpty(ptr,size)
 */
#ifdef HashSet

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "bitfiddle.h"
#include "util.h"
#include "xmalloc.h"

/* quadratic probing */
#ifndef JUMP
#define JUMP(num_probes)   (num_probes)
#endif

#ifndef Hash
#define ID_HASH
#define Hash(value)        ((unsigned)(value))
#endif

#ifdef DO_REHASH
#define HashSetEntry                 ValueType
#define EntrySetHash(entry,new_hash)
#define EntryGetHash(entry)          Hash(entry)
#define EntryGetValue(entry)         (entry)
#else
#define EntryGetHash(entry)          (entry).hash
#define EntrySetHash(entry,new_hash) (entry).hash = (new_hash)
#define EntryGetValue(entry)         (entry).data
#endif

#ifndef Alloc
#define Alloc(size)    (HashSetEntry*) xmalloc((size) * sizeof(HashSetEntry))
#define Free(ptr)      free(ptr)
#endif

#ifdef ID_HASH
#define InsertReturnValue           void
#define GetInsertReturnValue(entry)
#else
#define InsertReturnValue           ValueType
#define GetInsertReturnValue(entry) EntryGetValue(entry)
#endif

#ifndef KeyType
#define KeyType                  ValueType
#define GetKey(value)            (value)
#define InitData(this,value,key) (value) = (key)
#endif

#ifndef EntrySetEmpty
#define EntrySetEmpty(entry)     EntryGetValue(entry) = NullValue
#endif
#ifndef EntrySetDeleted
#define EntrySetDeleted(entry)   EntryGetValue(entry) = DeletedValue
#endif
#ifndef EntryIsEmpty
#define EntryIsEmpty(entry)      (EntryGetValue(entry) == NullValue)
#endif
#ifndef EntryIsDeleted
#define EntryIsDeleted(entry)    (EntryGetValue(entry) == DeletedValue)
#endif
#ifndef SetRangeEmpty
#define SetRangeEmpty(ptr,size)                \
{                                              \
	size_t _i;                                 \
	size_t _size = (size);                     \
	HashSetEntry *entries = (ptr);             \
	for(_i = 0; _i < _size; ++_i) {            \
		HashSetEntry *entry = & entries[_i];   \
		EntrySetEmpty(*entry);                 \
	}                                          \
}
#endif

#ifndef HT_OCCUPANCY_FLT
/** how full before we double size */
#define HT_OCCUPANCY_FLT  0.5f
#endif

#ifndef HT_EMPTY_FLT
/** how empty before we half size */
#define HT_EMPTY_FLT      (0.4f * (HT_OCCUPANCY_FLT))
#endif

#ifndef HT_MIN_BUCKETS
/** default smallest bucket size */
#define HT_MIN_BUCKETS    32
#endif

#define ILLEGAL_POS       ((size_t) -1)

size_t hashset_size(const HashSet *this)
{
	return this->num_elements - this->num_deleted;
}

static inline
InsertReturnValue insert_nogrow(HashSet *this, KeyType key)
{
	size_t num_probes = 0;
	size_t num_buckets = this->num_buckets;
	size_t hashmask = num_buckets - 1;
	unsigned hash = Hash(key);
	size_t bucknum = hash & hashmask;
	size_t insert_pos = ILLEGAL_POS;

	while(1) {
		HashSetEntry *entry = & this->entries[bucknum];

		if(EntryIsEmpty(*entry)) {
			size_t p;
			HashSetEntry *nentry;

			if(insert_pos != ILLEGAL_POS) {
				p = insert_pos;
			} else {
				p = bucknum;
			}

			nentry = &this->entries[p];
			InitData(this, EntryGetValue(*nentry), key);
			EntrySetHash(*nentry, hash);
			this->num_elements++;
			return GetInsertReturnValue(*nentry);
		}
		if(EntryIsDeleted(*entry)) {
			if(insert_pos == ILLEGAL_POS)
				insert_pos = bucknum;
		} else if(EntryGetHash(*entry) == hash) {
			if(KeysEqual(GetKey(EntryGetValue(*entry)), key)) {
				// Value already in the set, return it
				return GetInsertReturnValue(*entry);
			}
		}

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

static
void insert_new(HashSet *this, ValueType value)
{
	size_t num_probes = 0;
	size_t num_buckets = this->num_buckets;
	size_t hashmask = num_buckets - 1;
	unsigned hash = Hash(GetKey(value));
	size_t bucknum = hash & hashmask;
	size_t insert_pos = ILLEGAL_POS;

	assert(value != NullValue);

	while(1) {
		HashSetEntry *entry = & this->entries[bucknum];

		if(EntryIsEmpty(*entry)) {
			size_t p;
			HashSetEntry *nentry;

			if(insert_pos != ILLEGAL_POS) {
				p = insert_pos;
			} else {
				p = bucknum;
			}
			nentry = &this->entries[p];

			EntryGetValue(*nentry) = value;
			EntrySetHash(*nentry, hash);
			this->num_elements++;
			return;
		}
		assert(!EntryIsDeleted(*entry));

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

/**
 * calculate shrink and enlarge limits
 */
static inline
void reset_thresholds(HashSet *this)
{
	this->enlarge_threshold = (size_t) (this->num_buckets * HT_OCCUPANCY_FLT);
	this->shrink_threshold = (size_t) (this->num_buckets * HT_EMPTY_FLT);
	this->consider_shrink = 0;
}

static inline
void resize(HashSet *this, size_t new_size)
{
	size_t num_buckets = this->num_buckets;
	size_t i;
	HashSetEntry *old_entries = this->entries;
	HashSetEntry *new_entries;

	/* allocate a new array with double size */
	new_entries = Alloc(new_size);
	SetRangeEmpty(new_entries, new_size);

	/* use the new array */
	this->entries = new_entries;
	this->num_buckets = new_size;
	this->num_elements = 0;
	this->num_deleted = 0;
#ifndef NDEBUG
	this->entries_version++;
#endif
	reset_thresholds(this);

	/* reinsert all elements */
	for(i = 0; i < num_buckets; ++i) {
		HashSetEntry *entry = & old_entries[i];
		if(EntryIsEmpty(*entry) || EntryIsDeleted(*entry))
			continue;

		insert_new(this, EntryGetValue(*entry));
	}

	/* now we can free the old array */
	Free(old_entries);
}

/* grow the hashthis if adding 1 more elements would make it too crowded */
static inline
void maybe_grow(HashSet *this)
{
	size_t resize_to;

	if(LIKELY(this->num_elements + 1 <= this->enlarge_threshold))
		return;

	/* double table size */
	resize_to = this->num_buckets * 2;
	resize(this, resize_to);
}

static inline
void maybe_shrink(HashSet *this)
{
	size_t size;
	size_t resize_to;

	if(!this->consider_shrink)
		return;

	this->consider_shrink = 0;
	size = hashset_size(this);
	if(LIKELY(size > this->shrink_threshold))
		return;

	resize_to = ceil_po2(size);
	resize(this, resize_to);
}

InsertReturnValue hashset_insert(HashSet *this, KeyType key)
{
#ifndef NDEBUG
	this->entries_version++;
#endif

	maybe_shrink(this);
	maybe_grow(this);
	return insert_nogrow(this, key);
}

ValueType hashset_find(const HashSet *this, const KeyType key)
{
	size_t num_probes = 0;
	size_t num_buckets = this->num_buckets;
	size_t hashmask = num_buckets - 1;
	unsigned hash = Hash(key);
	size_t bucknum = hash & hashmask;

	while(1) {
		HashSetEntry *entry = & this->entries[bucknum];

		if(EntryIsEmpty(*entry)) {
			return NullValue;
		}
		if(EntryIsDeleted(*entry)) {
			// value is deleted
		} else if(EntryGetHash(*entry) == hash) {
			if(KeysEqual(GetKey(EntryGetValue(*entry)), key)) {
				// found the value
				return EntryGetValue(*entry);
			}
		}

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

void hashset_remove(HashSet *this, const KeyType key)
{
	size_t num_probes = 0;
	size_t num_buckets = this->num_buckets;
	size_t hashmask = num_buckets - 1;
	unsigned hash = Hash(key);
	size_t bucknum = hash & hashmask;

#ifndef NDEBUG
	this->entries_version++;
#endif

	while(1) {
		HashSetEntry *entry = & this->entries[bucknum];

		if(EntryIsEmpty(*entry)) {
			return;
		}
		if(EntryIsDeleted(*entry)) {
			// entry is deleted
		} else if(EntryGetHash(*entry) == hash) {
			if(KeysEqual(GetKey(EntryGetValue(*entry)), key)) {
				EntrySetDeleted(*entry);
				this->num_deleted++;
				this->consider_shrink = 1;
				return;
			}
		}

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

static inline
void init_size(HashSet *this, size_t initial_size)
{
	this->entries = Alloc(initial_size);
	SetRangeEmpty(this->entries, initial_size);
	this->num_buckets = initial_size;
	this->consider_shrink = 0;
	this->num_elements = 0;
	this->num_deleted = 0;
#ifndef NDEBUG
	this->entries_version = 0;
#endif

	reset_thresholds(this);
}

void hashset_init(HashSet *this)
{
	init_size(this, HT_MIN_BUCKETS);
}

void hashset_destroy(HashSet *this)
{
	Free(this->entries);
#ifndef NDEBUG
	this->entries = NULL;
#endif
}

void hashset_init_size(HashSet *this, size_t expected_elements)
{
	size_t needed_size;
	size_t po2size;

	if(expected_elements >= UINT_MAX/2) {
		abort();
	}
	
	needed_size = expected_elements * (1.0 / HT_OCCUPANCY_FLT);
	po2size = ceil_po2(needed_size);
	init_size(this, po2size);
}

void hashset_iterator_init(HashSetIterator *this, const HashSet *hashset)
{
	this->current_bucket = hashset->entries;
	this->end = hashset->entries + hashset->num_buckets;
#ifndef NDEBUG
	this->set = hashset;
	this->entries_version = hashset->entries_version;
#endif
}

ValueType hashset_iterator_next(HashSetIterator *this)
{
	HashSetEntry *current_bucket = this->current_bucket;
	ValueType res;
	HashSetEntry *next;
	HashSetEntry *end;

	if(current_bucket >= this->end)
		return NullValue;

	/* using hashset_insert or hashset_remove is not allowed while iterating */
	assert(this->entries_version == this->set->entries_version);

	end = this->end;
	res = EntryGetValue(*current_bucket);
	next = current_bucket + 1;

	while(next < end && (EntryIsEmpty(*next) || EntryIsDeleted(*next))) {
		next++;
	}
	this->current_bucket = next;

	return res;
}

#endif
