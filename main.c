#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <libfirm/be.h>
#include <libfirm/firm.h>

#include "type.h"
#include "parser.h"
#include "ast_t.h"
#include "semantic.h"
#include "ast2firm.h"
#include "plugins.h"
#include "type_hash.h"
#include "adt/error.h"

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
void get_output_name(char *buf, size_t buflen, const char *inputname,
                     const char *newext)
{
	size_t last_dot = 0xffffffff;
	size_t i = 0;
	for(const char *c = inputname; *c != 0; ++c) {
		if(*c == '.')
			last_dot = i;
		++i;
	}
	if(last_dot == 0xffffffff)
		last_dot = i;
	
	if(last_dot >= buflen)
		panic("filename too long");
	memcpy(buf, inputname, last_dot);

	size_t extlen = strlen(newext) + 1;
	if(extlen + last_dot >= buflen)
		panic("filename too long");
	memcpy(buf+last_dot, newext, extlen);
}

static
void backend(const char *inputname)
{
	char outfname[4096];

	get_output_name(outfname, sizeof(outfname), inputname, ".s");
	FILE *out = fopen(outfname, "w");
	if(out == NULL) {
		fprintf(stderr, "Couldn't open '%s': %s\n", outfname,
				strerror(errno));
		exit(1);
	}

	be_main(out, inputname);
}

static
void dump_ast(const namespace_t *namespace, const char *name)
{
	const char *fname = namespace->filename;
	char        filename[4096];
	get_output_name(filename, sizeof(filename), fname, name);

	FILE* out = fopen(filename, "w");
	if(out == NULL) {
		fprintf(stderr, "Warning: couldn't open '%s': %s\n", filename,
		        strerror(errno));
	} else {
		print_ast(out, namespace);
	}
	fclose(out);
}

static
void parse_file(const char *fname)
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
	dump_ast(namespace, "-parse.txt");

	namespace->next = namespaces;
	namespaces      = namespace;
}

static
void check_semantic_emit()
{
	namespace_t *namespace = namespaces;
	while(namespace != NULL) {
		if(!check_static_semantic(namespace)) {
			fprintf(stderr, "Semantic errors found\n");
			dump_ast(namespace, "-error.txt");
			exit(1);
		}
		dump_ast(namespace, "-semantic.txt");

		namespace = namespace->next;
	}

	namespace = namespaces;
	while(namespace != NULL) {
		ast2firm(namespace);

		namespace = namespace->next;
	}

    lower_highlevel();

	optimize();

	const char *fname = namespaces->filename;
	backend(fname);
}

int main(int argc, char **argv)
{
	if(argc < 2) {
		fprintf(stderr, "no input files.\n");
		return 0;
	}

	initialize_firm();
	init_symbol_table();
	init_tokens();
	init_type_module();
	init_typehash();
	init_ast_module();
	init_parser();
	init_semantic_module();
	search_plugins();
	initialize_plugins();

	for(int i = 1; i < argc; ++i) {
		parse_file(argv[i]);
	}

	check_semantic_emit();

	free_plugins();
	exit_semantic_module();
	exit_parser();
	exit_ast_module();
	exit_type_module();
	exit_typehash();
	exit_tokens();
	exit_symbol_table();
	exit_firm();

	return 0;
}

