#include <stdlib.h>
#include <stdio.h>

#include "adt/pset.h"

int main()
{
	pset_t pset;

	pset_init(&pset);
	pset_insert(&pset, (void*) main);
	printf("Contains main: %d, NULL: %d\n", pset_contains(&pset, (void*) main),
			pset_contains(&pset, NULL));
	pset_remove(&pset, NULL);
	printf("Size: %d\n", pset_size(&pset));

	pset_destroy(&pset);

	return 0;
}
