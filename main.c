#include <stdlib.h>
#include <stdio.h>

#include <obstack.h>
#include "adt/pset.h"
#include "adt/strset.h"
#include "adt/xmalloc.h"

static inline __attribute__((malloc))
void *obstack_chunk_alloc(size_t size) {
	return xmalloc(size);
}

static inline
void obstack_chunk_free(void *ptr) {
	free(ptr);
}

void test_pset()
{
	pset_t pset;
	pset_iterator_t iter;
	void *p;
	size_t i;

	srand(12345);
	pset_init(&pset);
	for(i = 0; i < 1000000; ++i) {
		if(rand() % 3) {
			pset_insert(&pset, (void*) (rand() % 10000)+1);
		} else {
			pset_remove(&pset, (void*) (rand() % 10000)+1);
		}
	}
	pset_remove(&pset, NULL);
	printf("Size: %d\n", pset_size(&pset));

#if 0
	pset_iterator_init(&iter, &pset);
	while( (p = pset_iterator_next(&iter)) != NULL) {
		printf("Entry: %p\n", p);
	}
#endif
	(void) p;
	(void) iter;

	pset_destroy(&pset);
}

char* readline(FILE *f, struct obstack *obst)
{
	while(1) {
		int c = fgetc(f);
		if(c == EOF)
			break;
		if(c == '\n')
			break;

		obstack_1grow(obst, c);
	}
	obstack_1grow(obst, '\0');

	return obstack_finish(obst);
}

void test_strset()
{
	strset_t set;
	FILE *in;
	struct obstack obst;

	obstack_init(&obst);
	strset_init(&set);

	in = fopen("words", "r");
	if(in == NULL) {
		perror("Couldn't open words");
	}

	while(!feof(in)) {
		char* word = readline(in, &obst);
		const char* setelem = strset_insert(&set, word);
		if(setelem != word) {
			printf("Freeing word '%s': already in set\n", word);
			obstack_free(&obst, word);
		}
	}

	strset_insert(&set, "Blup");
	strset_insert(&set, "bla");
	strset_insert(&set, "didum");

	strset_remove(&set, "gibtsnet");
	strset_remove(&set, "didum");

	printf("Contains Blup: %s\n", strset_find(&set, "Blup"));
	printf("Contains the: %s\n", strset_find(&set, "the"));

	fclose(in);
	strset_destroy(&set);
	obstack_free(&obst, NULL);
}

int main()
{
	//test_strset();
	test_pset();

	return 0;
}

