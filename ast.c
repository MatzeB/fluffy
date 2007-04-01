#include <config.h>

#include "ast_t.h"

#include <assert.h>
#include <stdlib.h>

#if 0
static environment_t *top_environment = NULL;

void push_environment(environment_t *environment)
{
	/* we could also copy all entries and allow this later... */
	assert(!environment->on_stack);

	/* put on environment stack */
	environment->down = top_environment;
	if(top_environment != NULL) {
		top_environment->up = environment;
	}
	top_environment = environment;

	/* put all symbols on the symbol stacks */
	environment_entry_t *entry;
	for(entry = environment->entries; entry != NULL; entry = entry->next) {
		symbol_t              *symbol = entry->symbol;
		environment_entry_t  *current = symbol->thing;

		entry->down = current;
		if(current != NULL) {
			current->up = entry;
		}
		symbol->thing = entry;
	}

	environment->on_stack = 1;
}

void pop_environment(environment_t *environment)
{
	assert(top_environment == environment);
	assert(environment->on_stack);
	
	/* remove from environment stack */
	top_environment = environment->up;
	top_environment->down = NULL;
	environment->up = NULL;
	environment->down = NULL;

	/* remove symbols from the symbol stacks */
	environment_entry_t *entry;
	for(entry = environment->entries; entry != NULL; entry = entry->next) {
		symbol_t              *symbol = entry->symbol;
		environment_entry_t  *current = entry->up;

		current->down = NULL;
		entry->up = NULL;
		entry->down = NULL;

		symbol->thing = current;
	}

	environment->on_stack = 0;
}

#endif
