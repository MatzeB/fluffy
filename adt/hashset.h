#ifndef _FIRM_HASHSET_H_
#define _FIRM_HASHSET_H_

#include <stdlib.h>

/* You have to specialize this header by defining HashSet, HashSetIterator and
 * ValueType */
#ifdef HashSet

typedef struct HashSet {
	ValueType *entries;
	size_t num_buckets;
	size_t enlarge_threshold;
	size_t shrink_threshold;
	size_t num_elements;
	size_t num_deleted;
	int consider_shrink;
#ifndef NDEBUG
	unsigned entries_version;
#endif
} HashSet;

typedef struct HashSetIterator {
	ValueType *current_bucket;
	ValueType *end;
#ifndef NDEBUG
	const HashSet *set;
	unsigned entries_version;
#endif
} HashSetIterator;

#endif

#endif

