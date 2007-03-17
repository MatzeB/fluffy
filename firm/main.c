#include <stdlib.h>
#include <stdio.h>

#include "pset.h"

int main()
{
	pset* pset;
	size_t i;

	srand(12345);
	pset = pset_new_ptr_default();
	pset_insert_ptr(pset, (void*) main);
	printf("Contains main: %d\n",
			pset_find_ptr(pset, (void*) main) != NULL);
	for(i = 0; i < 1000000; ++i) {
		if(rand() % 2) {
			pset_insert_ptr(pset, (void*) (rand() % 80)+1);
		} else {
			pset_remove_ptr(pset, (void*) (rand() % 80)+1);
		}
	}
	pset_remove_ptr(pset, NULL);
	printf("Size: %d\n", pset_count(pset));

	del_pset(pset);

	return 0;
}
