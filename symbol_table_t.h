#ifndef _SYMBOL_TABLE_T_H_
#define _SYMBOL_TABLE_T_H_

#include "adt/obst.h"

struct symbol_table_entry_t {
	const char *symbol;
	unsigned ID;
	/* additional stuff */
};
typedef struct symbol_table_entry_t symbol_table_entry_t;

#define HashSet          symbol_table_t
#define HashSetIterator  symbol_table_iterator_t
#define HashSetEntry     symbol_table_hash_entry_t
#define ValueType        symbol_table_entry_t*
#define ADDITIONAL_DATA  struct obstack obst
#include "adt/hashset.h"
#undef ADDITIONAL_DATA
#undef ValueType
#undef HashSetEntry
#undef HashSetIterator
#undef HashSet

#include "symbol_table.h"

#endif

