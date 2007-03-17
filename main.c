#include <stdlib.h>
#include <stdio.h>

#include "adt/pset.h"

int main()
{
	pset_t pset;
	size_t i;

	srand(12345);
	pset_init(&pset);
	pset_insert(&pset, (void*) main);
	printf("Contains main: %d, NULL: %d\n", pset_contains(&pset, (void*) main),
			pset_contains(&pset, NULL));
	for(i = 0; i < 1000000; ++i) {
		if(rand() % 3) {
			pset_insert(&pset, (void*) (rand() % 10000)+1);
		} else {
			pset_remove(&pset, (void*) (rand() % 10000)+1);
		}
	}
	pset_remove(&pset, NULL);
	printf("Size: %d\n", pset_size(&pset));

	pset_destroy(&pset);

	return 0;
}
