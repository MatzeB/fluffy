#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#define WITH_LIBCORE
#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "type.h"
#include "parser.h"
#include "ast_t.h"
#include "semantic.h"
#include "ast2firm.h"
#include "plugins.h"
#include "type_hash.h"
#include "adt/error.h"

#ifdef _WIN32
#define LINKER "gcc.exe"
#define TMPDIR ""
#else
#define LINKER "gcc"
#define TMPDIR "/tmp/"
#endif

static int dump_graphs = 0;
static int dump_asts   = 0;
static int verbose     = 0;

typedef enum compile_mode_t {
	Compile,
	CompileAndLink
} compile_mode_t;

const ir_settings_if_conv_t *if_conv_info = NULL;

static
void initialize_firm(void)
{
	be_opt_register();
	firm_init_options(NULL, 0, NULL);

	const backend_params *be_params;
	firm_parameter_t params;
	memset(&params, 0, sizeof(params));

	params.size = sizeof(params);
	params.enable_statistics = 0;
	params.initialize_local_func = uninitialized_local_var;
	params.cc_mask = 0;
	params.builtin_dbg = NULL;

	/* initialize backend */
	be_params = be_init();
	be_set_debug_retrieve(retrieve_dbg);
	params.arch_op_settings = be_params->arch_op_settings;
	if_conv_info            = be_params->if_conv_info;

	/* intialize firm itself */
	init_firm(&params);
	dbg_init(NULL, NULL, dbg_snprint);

	set_opt_constant_folding(1);
	set_opt_unreachable_code(1);
	set_opt_control_flow_straightening(1);
	set_opt_control_flow_weak_simplification(1);
	set_opt_control_flow_strong_simplification(1);
	set_opt_dead_node_elimination(1);
	set_opt_reassociation(1);
	set_opt_inline(1);
	set_opt_dyn_meth_dispatch(1);
	set_opt_normalize(1);
	set_opt_tail_recursion(1);
	set_opt_dead_method_elimination(1);
	set_opt_precise_exc_context(0);
	set_opt_loop_unrolling(0);
	set_opt_strength_red(0);
	set_opt_redundant_loadstore(1);
	set_opt_fragile_ops(0);
	set_opt_function_call(1);
	set_opt_optimize_class_casts(0);
	set_opt_suppress_downcast_optimization(0);
	set_opt_remove_confirm(1);
	set_opt_scalar_replacement(1);
	set_opt_ldst_only_null_ptr_exceptions(1);
	set_opt_alias_analysis(1);

	dump_consts_local(1);
}

static
void dump(const char *suffix)
{
	if(!dump_graphs)
		return;
	dump_ir_block_graph(current_ir_graph, suffix);
}

static
void optimize()
{
	int i;
	int n_irgs = get_irp_n_irgs();
	for(i = 0; i < n_irgs; ++i) {
		current_ir_graph = get_irp_irg(i);
		dump("-begin");
	}

	set_irp_memory_disambiguator_options(aa_opt_type_based | aa_opt_byte_type_may_alias);

	int arr_len;
	ir_entity **keep_methods;
	cgana(&arr_len, &keep_methods);
	gc_irgs(arr_len, keep_methods);
	free(keep_methods);

	current_ir_graph = NULL;
	opt_tail_recursion();

	optimize_funccalls(0);
	inline_leave_functions(500, 80, 30, 0);

	for(i = 0; i < get_irp_n_irgs(); ++i) {
		ir_graph *irg = get_irp_irg(i);

		current_ir_graph = irg;

		dump("-lower");

		/* TODO: improve this and make it configurabble */
		scalar_replacement_opt(irg);
		optimize_graph_df(irg);
		optimize_reassociation(irg);
		optimize_cf(irg);
		construct_confirms(irg);
		optimize_graph_df(irg);
		compute_doms(irg);

		set_opt_global_cse(1);
		optimize_graph_df(irg);
		place_code(irg);
		set_opt_global_cse(0);

		dump("-after_gcse");

		optimize_cf(irg);
		remove_confirms(irg);
		
		optimize_load_store(irg);
		conv_opt(irg);

		dump("-before_condeval");

		opt_cond_eval(irg);


		dump("-after_condeval");

		compute_doms(irg);
		compute_postdoms(irg);
		construct_backedges(irg);

		optimize_cf(irg);
		opt_if_conv(irg, if_conv_info);
		optimize_graph_df(irg);
		optimize_cf(irg);
		optimize_graph_df(irg);
		dead_node_elimination(irg);

		dump("-opt-run1");

		compute_doms(irg);
		compute_postdoms(irg);

		set_opt_global_cse(1);
		optimize_graph_df(irg);
		place_code(irg);
		set_opt_global_cse(0);

		dump("-after_gcse2");

		optimize_cf(irg);
		remove_confirms(irg);
		
		optimize_load_store(irg);
		conv_opt(irg);
	
		dump("-opt-run2");
	}

	cgana(&arr_len, &keep_methods);
	gc_irgs(arr_len, keep_methods);
	free(keep_methods);
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
void backend(const char *inputname, const char *outname)
{
	FILE *out = fopen(outname, "w");
	if(out == NULL) {
		fprintf(stderr, "couldn't open '%s' for writing: %s\n", outname,
				strerror(errno));
		exit(1);
	}

	be_main(out, inputname);

	fclose(out);
}

static
void dump_ast(const namespace_t *namespace, const char *name)
{
	if(!dump_asts)
		return;

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
		fprintf(stderr, "couldn't open '%s' for reading: %s\n", fname,
		        strerror(errno));
		exit(1);
	}

	namespace_t *namespace = parse(in, fname);
	fclose(in);

	if(namespace == NULL) {
		exit(1);
	}
	dump_ast(namespace, "-parse.txt");
}

static
void check_semantic(void)
{
	if(!check_static_semantic()) {
		fprintf(stderr, "Semantic errors found\n");
		namespace_t *namespace = namespaces;
		while(namespace != NULL) {
			dump_ast(namespace, "-error.txt");
			namespace = namespace->next;
		}
		exit(1);
	}

	if(dump_asts) {
		namespace_t *namespace = namespaces;
		while(namespace != NULL) {
			dump_ast(namespace, "-semantic.txt");
			namespace = namespace->next;
		}
	}
}

static
void emit(const char *outname)
{
	char outfname[4096];

	ast2firm();
    lower_highlevel();

	optimize();

	const char *fname = namespaces->filename;
	if(outname == NULL) {
		get_output_name(outfname, sizeof(outfname), fname, ".s");
		outname = outfname;
	}

	backend(fname, outname);
}

static
void link(const char *in, const char *out)
{
	char buf[4096];

	if(out == NULL) {
		out = "a.out";
	}

	snprintf(buf, sizeof(buf), "%s %s -o %s", LINKER, in, out);
	if(verbose) {
		puts(buf);
	}
	int err = system(buf);
	if(err != 0) {
		fprintf(stderr, "linker reported an error\n");
		exit(1);
	}
}

static
void usage(const char *argv0)
{
	fprintf(stderr, "Usage: %s input1 input2 [-o output]\n", argv0);
}

int main(int argc, const char **argv)
{
	init_symbol_table();
	init_tokens();
	init_type_module();
	init_typehash();
	init_ast_module();
	init_parser();
	init_semantic_module();
	search_plugins();
	initialize_plugins();
	initialize_firm();
	init_ast2firm();

	const char *outname = NULL;
	compile_mode_t mode = CompileAndLink;
	int parsed = 0;

	for(int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if(strcmp(arg, "-o") == 0) {
			++i;
			if(i >= argc) {
				usage(argv[0]);
				return 1;
			}
			outname = argv[i];
		} else if(strcmp(arg, "--dump") == 0) {
			dump_graphs = 1;
			dump_asts   = 1;
		} else if(strcmp(arg, "--dump-ast") == 0) {
			dump_asts = 1;
		} else if(strcmp(arg, "--dump-graph") == 0) {
			dump_graphs = 1;
		} else if(strcmp(arg, "--help") == 0) {
			usage(argv[0]);
			return 0;
		} else if(strcmp(arg, "-S") == 0) {
			mode = Compile;
		} else if(strcmp(arg, "-v") == 0) {
			verbose = 1;
		} else if(strncmp(arg, "-b", 2) == 0) {
			const char *bearg = arg+2;
			if(bearg[0] == 0) {
				++i;
				if(i >= argc) {
					usage(argv[0]);
					return 1;
				}
				bearg = argv[i];
			}
			if(!be_parse_arg(bearg)) {
				fprintf(stderr, "Invalid backend option: %s\n", bearg);
				usage(argv[0]);
				return 1;
			}
			if(strcmp(bearg, "help") == 0) {
				return 1;
			}
		} else {
			parsed++;
			parse_file(argv[i]);
		}
	}
	if(parsed == 0) {
		fprintf(stderr, "Error: no input files specified\n");
		return 0;
	}

	check_semantic();
	const char *asmname;
	if(mode == Compile) {
		asmname = outname;
	} else {
		asmname = TMPDIR "fluffy.s";
	}
	emit(asmname);
	if(mode == CompileAndLink) {
		link(asmname, outname);
	}

	exit_ast2firm();
	free_plugins();
	exit_semantic_module();
	exit_parser();
	exit_ast_module();
	exit_type_module();
	exit_typehash();
	exit_tokens();
	exit_symbol_table();

	return 0;
}

