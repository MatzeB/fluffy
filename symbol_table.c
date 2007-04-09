#include <config.h>

#include "symbol_table_t.h"
#include "adt/hash_string.h"
#include "adt/obst.h"

static inline
void init_symbol_table_entry(symbol_t *entry, const char *string)
{
	entry->ID     = 0;
	entry->string = string;
	entry->thing  = NULL;
	entry->label  = NULL;
}

#define HashSet                    symbol_table_t
#define HashSetIterator            symbol_table_iterator_t
#define HashSetEntry               symbol_table_hash_entry_t
#define ValueType                  symbol_t*
#define NullValue                  NULL
#define DeletedValue               ((symbol_t*)-1)
#define KeyType                    const char *
#define ConstKeyType               const char *
#define GetKey(value)              (value)->string
#define InitData(this,value,key)   { (value) = (ValueType) obstack_alloc(&this->obst, sizeof(symbol_t)); init_symbol_table_entry((value), key); }
#define Hash(this, key)            hash_string(key)
#define KeysEqual(this,key1,key2)  (strcmp(key1, key2) == 0)
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(symbol_table_hash_entry_t))

#define hashset_init            _symbol_table_init
#define hashset_init_size       _symbol_table_init_size
#define hashset_destroy         _symbol_table_destroy
#define hashset_insert          symbol_table_insert
#define hashset_remove          _symbol_table_remove
#define hashset_find            _symbol_table_find
#define hashset_size            _symbol_table_size
#define hashset_iterator_init   _symbol_table_iterator_init
#define hashset_iterator_next   _symbol_table_iterator_next
#define hashset_remove_iterator _symbol_table_remove_iterator

#include "adt/hashset.c"

void symbol_table_init(symbol_table_t *this)
{
	obstack_init(&this->obst);
	_symbol_table_init(this);
}

void symbol_table_destroy(symbol_table_t *this)
{
	_symbol_table_destroy(this);
	obstack_free(&this->obst, NULL);
}

#include <stdio.h>
void iter_test(symbol_table_t *this)
{
	symbol_table_iterator_t iter;
	symbol_t *entry;

	_symbol_table_iterator_init(&iter, this);
	while ((entry = _symbol_table_iterator_next(&iter)) != NULL) {
		printf("Entry: '%s'\n", entry->string);
	}
}
