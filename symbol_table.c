#include <config.h>

#include "symbol_table_t.h"
#include "adt/hash_string.h"
#include "adt/obst.h"

struct obstack symbol_obstack;

static inline void init_symbol_table_entry(symbol_t *entry, const char *string)
{
	entry->ID          = 0;
	entry->string      = string;
	entry->declaration = NULL;
	entry->context     = NULL;
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
#define InitData(this,value,key)   { (value) = (ValueType) obstack_alloc(&symbol_obstack, sizeof(symbol_t)); init_symbol_table_entry((value), key); }
#define Hash(this, key)            hash_string(key)
#define KeysEqual(this,key1,key2)  (strcmp(key1, key2) == 0)
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(symbol_table_hash_entry_t))
#define SCALAR_RETURN

#define hashset_init            _symbol_table_init
#define hashset_init_size       _symbol_table_init_size
#define hashset_destroy         _symbol_table_destroy
#define hashset_insert          _symbol_table_insert
#define hashset_remove          _symbol_table_remove
#define hashset_find            _symbol_table_find
#define hashset_size            _symbol_table_size
#define hashset_iterator_init   _symbol_table_iterator_init
#define hashset_iterator_next   _symbol_table_iterator_next
#define hashset_remove_iterator _symbol_table_remove_iterator

#include "adt/hashset.c"

static symbol_table_t  real_symbol_table;
symbol_table_t        *symbol_table = NULL;

symbol_t *symbol_table_insert(const char *symbol)
{
	return _symbol_table_insert(symbol_table, symbol);
}

void init_symbol_table(void)
{
	symbol_table_t *this = &real_symbol_table;
	obstack_init(&symbol_obstack);
	_symbol_table_init(this);

	symbol_table = this;
}

void exit_symbol_table(void)
{
	_symbol_table_destroy(symbol_table);
	obstack_free(&symbol_obstack, NULL);

	symbol_table = NULL;
}

