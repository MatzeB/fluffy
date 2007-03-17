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

static
void insert_nogrow(HashSet *this, ValueType value)
{
	size_t num_probes = 0;
	size_t num_buckets = this->num_buckets;
	size_t hashmask = num_buckets - 1;
	size_t bucknum = Hash(value) & hashmask;
	size_t insert_pos = ILLEGAL_POS;

	assert(value != EmptyEntry);
	assert(value != DeletedEntry);

	while(1) {
		ValueType v = this->entries[bucknum];

		if(IsEmptyEntry(v)) {
			size_t p;

			if(insert_pos != ILLEGAL_POS) {
				p = insert_pos;
			} else {
				p = bucknum;
			}
			this->entries[p] = value;
			this->num_elements++;
			return;
		}
		if(IsDeletedEntry(v)) {
			if(insert_pos == ILLEGAL_POS)
				insert_pos = bucknum;
		} else if(ValsEqual(v, value)) {
			// Value already in the set
			return;
		}

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
	ValueType *old_entries = this->entries;
	ValueType *new_entries;

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
		ValueType v = old_entries[i];
		if(IsEmptyEntry(v) || IsDeletedEntry(v))
			continue;

		insert_nogrow(this, v);
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

void hashset_insert(HashSet *this, ValueType value)
{
#ifndef NDEBUG
	this->entries_version++;
#endif

	maybe_shrink(this);
	maybe_grow(this);
	insert_nogrow(this, value);
}

ValueType hashset_find(const HashSet *this, const ValueType value)
{
	size_t num_probes = 0;
	size_t num_buckets = this->num_buckets;
	size_t hashmask = num_buckets - 1;
	size_t bucknum = Hash(value) & hashmask;

	while(1) {
		ValueType v = this->entries[bucknum];

		if(IsEmptyEntry(v)) {
			return EmptyEntry;
		}
		if(ValsEqual(v, value)) {
			// Value already in the set
			return v;
		}

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

void hashset_remove(HashSet *this, const ValueType value)
{
	size_t num_probes = 0;
	size_t num_buckets = this->num_buckets;
	size_t hashmask = num_buckets - 1;
	size_t bucknum = Hash(value) & hashmask;

#ifndef NDEBUG
	this->entries_version++;
#endif

	while(1) {
		ValueType v = this->entries[bucknum];

		if(IsEmptyEntry(v)) {
			return;
		}
		if(ValsEqual(v, value)) {
			this->entries[bucknum] = DeletedEntry;
			this->num_deleted++;
			this->consider_shrink = 1;
			return;
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
	ValueType *current_bucket = this->current_bucket;
	ValueType res;
	ValueType *next;
	ValueType *end;

	if(current_bucket >= this->end)
		return EmptyEntry;

	/* make sure hashset_insert or hashset_remove is not used while iterating */
	assert(this->entries_version == this->set->entries_version);

	end = this->end;
	res = * (this->current_bucket);
	next = res + 1;

	while(next < end && (IsEmptyEntry(*next) || IsDeletedEntry(*next))) {
		next++;
	}
	this->current_bucket = next;

	return res;
}

#endif
