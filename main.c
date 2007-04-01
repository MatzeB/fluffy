#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <firm/be/be.h>

#include "parser.h"
#include "semantic.h"
#include "ast2firm.h"

void test_parser(const char *fname)
{
	FILE *in = fopen(fname, "r");
	if(in == NULL) {
		fprintf(stderr, "Couldn't open '%s': %s\n", fname, strerror(errno));
		exit(1);
	}

	namespace_t *namespace = parse(in, fname);
	fclose(in);

	if(namespace == NULL) {
		exit(1);
	}

	check_static_semantic(namespace);
	ast2firm(namespace);

	const char* outfname = "out.s";
	FILE *out = fopen(outfname, "w");
	if(out == NULL) {
		fprintf(stderr, "Couldn't open '%s': %s\n", outfname,
				strerror(errno));
		exit(1);
	}

	be_main(out, fname);
}

int main(int argc, char **argv)
{
	initialize_firm();

	if(argc > 1) {
		test_parser(argv[1]);
	}

	exit_firm();

	return 0;
}

