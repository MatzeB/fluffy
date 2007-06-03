#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <libfirm/be.h>
#include <libfirm/firm.h>

#include "type.h"
#include "parser.h"
#include "semantic.h"
#include "ast2firm.h"
#include "plugins.h"

#define PRINT_AST

static
void optimize()
{
#if 1
	ir_entity **keep_methods;
	int arr_len;

	cgana(&arr_len, &keep_methods);
	gc_irgs(arr_len, keep_methods);
	free(keep_methods);

	optimize_funccalls(1);
	inline_leave_functions(500, 80, 30, 0);
#endif

	int i;
	int n_irgs      = get_irp_n_irgs();
	for(i = 0; i < n_irgs; ++i) {
		ir_graph *irg = get_irp_irg(i);

		dump_consts_local(1);
		dump_ir_block_graph(irg, "-lower");

		/* TODO: improve this and make it configurabble */

		optimize_graph_df(irg);
		optimize_reassociation(irg);
		optimize_cf(irg);
		construct_confirms(irg);
		optimize_graph_df(irg);
		set_opt_global_cse(1);
		optimize_graph_df(irg);
		place_code(irg);
		set_opt_global_cse(0);

		optimize_cf(irg);
		remove_confirms(irg);

		optimize_load_store(irg);

		dump_ir_block_graph(irg, "-opt");
	}

#if 1
	cgana(&arr_len, &keep_methods);
	gc_irgs(arr_len, keep_methods);
	free(keep_methods);
#endif
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

#ifdef PRINT_AST
	fprintf(stderr, "--------- After parsing: ------------\n");
	print_ast(stderr, namespace);
#endif

	if(!check_static_semantic(namespace)) {
		print_ast(stderr, namespace);
		fprintf(stderr, "Semantic errors found\n");
		exit(1);
	}

#ifdef PRINT_AST
	fprintf(stderr, "--------- After semantic analysis: ------------\n");
	print_ast(stderr, namespace);
#endif

	ast2firm(namespace);

    lower_highlevel();

	optimize();

	backend(fname);
}

int main(int argc, char **argv)
{
	int i;
	initialize_firm();
	init_type_module();
	search_plugins();

	for(i = 1; i < argc; ++i) {
		compile(argv[i]);
	}

	free_plugins();
	exit_firm();
	exit_type_module();

	return 0;
}
