#include <config.h>

#include "pset.h"

/** probing method: quadratic probing */
#define HashSet                 pset_t
#define HashSetIterator         pset_iterator_t
#define JUMP(num_probes)        (num_probes)
#define ValueType               void*
#define Hash(value)             ((int) (value))
#define ValsEqual(val1, val2)   ((val1) == (val2))
#define Alloc(size)             (ValueType*) xmalloc((size) * sizeof(ValueType))
#define Free(ptr)               free(ptr)
#define EmptyEntry              NULL
#define DeletedEntry            ((char*)-1)
#define IsEmptyEntry(entry)     ((entry) == EmptyEntry)
#define IsDeletedEntry(entry)   ((entry) == DeletedEntry)
#define SetRangeEmpty(ptr,size) memset(ptr, 0, (size) * sizeof(ValueType))

#define hashset_init          pset_init
#define hashset_init_size     pset_init_size
#define hashset_destroy       pset_destroy
#define hashset_insert        pset_insert
#define hashset_remove        pset_remove
#define hashset_find          pset_find
#define hashset_size          pset_size
#define hashset_iterator_init pset_iterator_init
#define hashset_iterator_next pset_iterator_next

#include "hashset.c"

int pset_contains(const pset_t *pset, const ValueType val)
{
	return pset_find(pset, val) != EmptyEntry;
}
