#ifndef _SYMBOL_TABLE_T_H_
#define _SYMBOL_TABLE_T_H_

#include "symbol_table.h"
#include "adt/obst.h"
#include "symbol.h"

typedef struct symbol_table_iterator_t symbol_table_iterator_t;

#define NO_TYPEDEFS
#define HashSet          symbol_table_t
#define HashSetIterator  symbol_table_iterator_t
#define HashSetEntry     symbol_table_hash_entry_t
#define ValueType        symbol_t*
#define ADDITIONAL_DATA  struct obstack obst
#include "adt/hashset.h"
#undef ADDITIONAL_DATA
#undef ValueType
#undef HashSetEntry
#undef HashSetIterator
#undef HashSet
#undef NO_TYPEDEFS

void symbol_table_init(symbol_table_t *symbol_table);

void symbol_table_destroy(symbol_table_t *symbol_table);

#endif

