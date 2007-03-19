/* You have to specialize this header by defining HashSet, HashSetIterator and
 * ValueType */
#ifdef HashSet

#include <stdlib.h>

#ifndef ADDITIONAL_DATA
#define ADDITIONAL_DATA
#endif

#ifdef DO_REHASH
#define HashSetEntry ValueType
#else
typedef struct HashSetEntry {
	ValueType data;
	unsigned hash;
} HashSetEntry;
#endif

typedef struct HashSet {
	HashSetEntry *entries;
	size_t num_buckets;
	size_t enlarge_threshold;
	size_t shrink_threshold;
	size_t num_elements;
	size_t num_deleted;
	int consider_shrink;
#ifndef NDEBUG
	unsigned entries_version;
#endif
	ADDITIONAL_DATA;
} HashSet;

typedef struct HashSetIterator {
	HashSetEntry *current_bucket;
	HashSetEntry *end;
#ifndef NDEBUG
	const HashSet *set;
	unsigned entries_version;
#endif
} HashSetIterator;

#ifdef DO_REHASH
#undef HashSetEntry
#endif

#endif

