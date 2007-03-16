#include <stdlib.h>
#include <assert.h>

#include "bitfiddle.h"
#include "util.h"
#include "xmalloc.h"

/** probing method: quadratic probing */
#define JUMP_(num_probes)       (num_probes)
#define ValueType               void*
#define Hash(value)             ((int) (value))
#define ValsEqual(val1, val2)   ((val1) == (val2))
#define Alloc(size)             (ValueType*) xmalloc((size) * sizeof(ValueType))
#define ReAlloc(ptr,size)       realloc(ptr, (size) * sizeof(ValueType))
#define Free(ptr)               free(ptr)
#define EmptyEntry              NULL
#define DeletedEntry            ((char*)-1)
#define IsEmptyEntry(entry)     ((entry) == EmptyEntry)
#define IsDeletedEntry(entry)   ((entry) == DeletedEntry)

/** how full before we double size */
#define HT_OCCUPANCY_FLT  0.5f
/** how empty before we half size */
#define HT_EMPTY_FLT      (0.4f * (HT_OCCUPANCY_FLT))
/** default smallest bucket size */
#define HT_MIN_BUCKETS    32

#define ILLEGAL_POS       ((size_t) -1)

typedef struct HashSet {
	ValueType *entries;
	size_t num_buckets;
	size_t enlarge_threshold;
	size_t shrink_threshold;
	size_t num_elements;
	size_t num_deleted;
	int consider_shrink;
} HashSet;

struct HashSetIterator {
	ValueType *i;
	ValueType *end;
};

/* grow the hashtable if adding 1 more elements would make it too crowded */
static inline
void maybe_grow(HashSet *table)
{
	size_t num_buckets = table->num_buckets;
	size_t resize_to;
	size_t i, np;
	size_t hashmask;
	ValueType *entries;

	if(LIKELY(num_buckets + 1 <= table->enlarge_threshold))
		return;

	resize_to = num_buckets * 2;
	entries = ReAlloc(table->entries, resize_to);

	/* Rehash the elements. At this point we use the fact that when doubling
	 * the table size each element either stays at his place or moves exactly
	 * the old table size upwards. (because it's just 1 more bit we consider in
	 * the hashmask)
	 */
	hashmask = resize_to - 1;
	np = num_buckets;
	for(i = 0; i < num_buckets; ++i, ++np) {
		ValueType val = entries[i];
		size_t bucket = Hash(val) & hashmask;

		if(bucket == i) {
			entries[np] = EmptyEntry;
		} else {
			assert(bucket == np);
			entries[np] = val;
			entries[i] = EmptyEntry;
		}
	}

	table->entries = entries;
}

static void reset_thresholds(HashSet *table)
{
	table->enlarge_threshold = (size_t) (table->num_buckets * HT_OCCUPANCY_FLT);
	table->shrink_threshold = (size_t) (table->num_buckets * HT_EMPTY_FLT);
	table->consider_shrink = 0;
}

static void insert_nogrow(HashSet *table, ValueType value)
{
	size_t num_probes = 0;
	size_t num_buckets = table->num_buckets;
	size_t hashmask = num_buckets - 1;
	size_t bucknum = Hash(value) & hashmask;
	size_t insert_pos = ILLEGAL_POS;

	while(1) {
		ValueType v = table->entries[bucknum];

		if(IsEmptyEntry(v)) {
			size_t p;

			if(insert_pos != ILLEGAL_POS) {
				p = insert_pos;
			} else {
				p = bucknum;
			}
			table->entries[p] = value;
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
		bucknum = (bucknum + JUMP_(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

void ht_insert(HashSet *table, ValueType value)
{
	maybe_grow(table);
	insert_nogrow(table, value);
}

int ht_contains(const HashSet *table, ValueType value)
{
	size_t num_probes = 0;
	size_t num_buckets = table->num_buckets;
	size_t hashmask = num_buckets - 1;
	size_t bucknum = Hash(value) & hashmask;

	while(1) {
		ValueType v = table->entries[bucknum];

		if(IsEmptyEntry(v)) {
			return 0;
		}
		if(ValsEqual(v, value)) {
			// Value already in the set
			return 1;
		}

		++num_probes;
		bucknum = (bucknum + JUMP_(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

void ht_remove(HashSet *table, ValueType value)
{
	size_t num_probes = 0;
	size_t num_buckets = table->num_buckets;
	size_t hashmask = num_buckets - 1;
	size_t bucknum = Hash(value) & hashmask;

	while(1) {
		ValueType v = table->entries[bucknum];

		if(IsEmptyEntry(v)) {
			return;
		}
		if(ValsEqual(v, value)) {
			table->entries[bucknum] = DeletedEntry;
			return;
		}

		++num_probes;
		bucknum = (bucknum + JUMP_(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

static inline
void init_size(HashSet *table, size_t initial_size)
{
	table->entries = Alloc(initial_size);
	table->num_buckets = initial_size;

	reset_thresholds(table);
}

void ht_init(HashSet *table)
{
	init_size(table, HT_MIN_BUCKETS);
}

void ht_init_size(HashSet *table, size_t expected_elements)
{
	size_t needed_size;
	size_t po2size;

	if(expected_elements >= UINT_MAX/2) {
		abort();
	}
	
	needed_size = expected_elements * (1.0 / HT_OCCUPANCY_FLT);
	po2size = ceil_po2(needed_size);
	init_size(table, po2size);
}

size_t ht_size(HashSet *table)
{
	return table->num_elements - table->num_deleted;
}

