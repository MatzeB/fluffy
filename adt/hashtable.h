#ifndef HASHTABLE_H_
#define HASHTABLE_H_

typedef void* HashEntryPtr;

struct HashTable {
	HashEntryPtr *entries;
	size_t size;
};

#endif

