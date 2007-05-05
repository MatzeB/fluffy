#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <firm/be/be.h>
#include <firm/ir/irprog.h>
#include <firm/ir/irgopt.h>
#include <firm/ir/iropt.h>
#include <firm/ir/irdump.h>
#include <firm/lower/lower_hl.h>

#include "parser.h"
#include "semantic.h"
#include "ast2firm.h"

static
void optimize()
{
	int i;
	int n_irgs      = get_irp_n_irgs();
	for(i = 0; i < n_irgs; ++i) {
		ir_graph *irg = get_irp_irg(i);

		dump_consts_local(1);
		dump_ir_block_graph(irg, "-lower");

		/* do some simple optimisations... */
		place_code(irg);
		optimize_graph_df(irg);
		dead_node_elimination(irg);

		dump_ir_block_graph(irg, "-opt");
	}
}

static
void backend(const char *inputname)
{
	const char* outfname = "out.s";
	FILE *out = fopen(outfname, "w");
	if(out == NULL) {
		fprintf(stderr, "Couldn't open '%s': %s\n", outfname,
				strerror(errno));
		exit(1);
	}

	be_main(out, inputname);
}

static
void compile(const char *fname)
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

	if(!check_static_semantic(namespace)) {
		fprintf(stderr, "Semantic errors found\n");
		exit(1);
	}
	ast2firm(namespace);

    lower_highlevel();

	optimize();

	backend(fname);
}

int main(int argc, char **argv)
{
	int i;
	initialize_firm();

	for(i = 1; i < argc; ++i) {
		compile(argv[i]);
	}

	exit_firm();

	return 0;
}
