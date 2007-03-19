#include <config.h> 

#include "symbol_table.h"
#include "adt/hash_string.h"
#include <obstack.h>

#define HashSet                  symbol_table_t
#define HashSetIterator          symbol_table_iterator_t
#define HashSetEntry             symbol_table_hash_entry_t
#define ValueType                symbol_table_entry_t*
#define NullValue                NULL
#define DeletedValue             ((void*)-1)
#define KeyType                  const char *
#define GetKey(value)            (value)->symbol
#define InitData(this,value,key) { (value) = obstack_alloc(this->obst, sizeof(ValueType)); (value)->symbol = key; }
#define Hash(key)               hash_string(key)
#define KeysEqual(key1, key2)   (strcmp(key1, key2) == 0)
//#define SetRangeEmpty(ptr,size) memset(ptr, 0, (size) * sizeof(strset_entry_t))

#define hashset_init          symbol_table_init
#define hashset_init_size     symbol_table_init_size
#define hashset_destroy       symbol_table_destroy
#define hashset_insert        symbol_table_insert
#define hashset_remove        symbol_table_remove
#define hashset_find          symbol_table_find
#define hashset_size          symbol_table_size
#define hashset_iterator_init symbol_table_iterator_init
#define hashset_iterator_next symbol_table_iterator_next

#include "adt/hashset.c"

