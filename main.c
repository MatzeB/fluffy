#include <config.h>

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <sys/time.h>

#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "driver/firm_opt.h"
#include "driver/firm_cmdline.h"
#include "driver/firm_machine.h"

#include "type.h"
#include "parser.h"
#include "ast_t.h"
#include "semantic.h"
#include "ast2firm.h"
#include "plugins.h"
#include "type_hash.h"
#include "mangle.h"
#include "adt/error.h"
#include "adt/strutil.h"

#define LINKER "gcc -m32"
#define TMPDIR "/tmp/"

static machine_triple_t *target_machine;
static bool dump_graphs;
static bool dump_asts;
static bool verbose;
static bool had_parse_errors;

typedef enum compile_mode_t {
	Compile,
	CompileAndLink
} compile_mode_t;

static void initialize_firm(void)
{
	firm_early_init();
}

static void get_output_name(char *buf, size_t buflen, const char *inputname,
                            const char *newext)
{
	size_t last_dot = 0xffffffff;
	size_t i = 0;
	for (const char *c = inputname; *c != 0; ++c) {
		if (*c == '.')
			last_dot = i;
		++i;
	}
	if (last_dot == 0xffffffff)
		last_dot = i;
	
	if (last_dot >= buflen)
		panic("filename too long");
	memcpy(buf, inputname, last_dot);

	size_t extlen = strlen(newext) + 1;
	if (extlen + last_dot >= buflen)
		panic("filename too long");
	memcpy(buf+last_dot, newext, extlen);
}

static void dump_ast(const context_t *context, const char *name,
                     const char *ext)
{
	if (!dump_asts)
		return;

	char filename[4096];
	get_output_name(filename, sizeof(filename), name, ext);

	FILE* out = fopen(filename, "w");
	if (out == NULL) {
		fprintf(stderr, "Warning: couldn't open '%s': %s\n", filename,
		        strerror(errno));
	} else {
		print_ast(out, context);
	}
	fclose(out);
}

static void do_parse_file(FILE *in, const char *input_name)
{
	bool result = parse_file(in, input_name);
	if (!result) {
		fprintf(stderr, "syntax errors found...\n");
		had_parse_errors = true;
		return;
	}
}

static void do_check_semantic(void)
{
	bool result = check_semantic();
	if (!result) {
		fprintf(stderr, "Semantic errors found\n");
		exit(1);
	}

	const module_t *module = modules;
	for ( ; module != NULL; module = module->next) {
		dump_ast(&module->context, module->name->string,
		         "-semantic.txt");
	}
}

static void link(const char *in, const char *out)
{
	char buf[4096];

	if (out == NULL) {
		out = "a.out";
	}

	int res = snprintf(buf, sizeof(buf), "%s %s -o %s", LINKER, in, out);
	if (res < 0 || res >= (int) sizeof(buf)) {
		panic("Couldn't construct linker commandline (too long?)");
	}
	if (verbose) {
		puts(buf);
	}
	int err = system(buf);
	if (err != 0) {
		fprintf(stderr, "linker reported an error\n");
		exit(1);
	}
}

static void usage(const char *argv0)
{
	fprintf(stderr, "Usage: %s input1 input2 [-o output]\n", argv0);
}

static void setup_target(void)
{
	const char *os = target_machine->operating_system;
	if (strstr(os, "linux") != NULL || strstr(os, "bsd") != NULL
			|| streq(os, "solaris")) {
		set_add_underscore_prefix(false);
	} else if (streq(os, "darwin") || strstr(os, "mingw") || strstr(os, "win32")) {
		set_add_underscore_prefix(true);
	} else {
		panic("Unsupported operating system ");
	}
}

int main(int argc, const char **argv)
{
	int opt_level = 2;

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
	init_mangle();

	/* early options parsing */
	for (int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if (arg[0] != '-')
			continue;

		const char *option = &arg[1];
		if (option[0] == 'O') {
			sscanf(&option[1], "%d", &opt_level);
		}
	}
	const char *target = getenv("TARGET");
	if (target != NULL)
		target_machine = firm_parse_machine_triple(target);
	if (target_machine == NULL)
		target_machine = firm_get_host_machine();
	choose_optimization_pack(opt_level);
	setup_firm_for_machine(target_machine);
	setup_target();

	const char *outname = NULL;
	compile_mode_t mode = CompileAndLink;
	int parsed = 0;

	for (int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if (strcmp(arg, "-o") == 0) {
			++i;
			if (i >= argc) {
				usage(argv[0]);
				return 1;
			}
			outname = argv[i];
		} else if (strstart(arg, "-O")) {
			/* already processed in first pass */
		} else if (strcmp(arg, "--dump") == 0) {
			dump_graphs = 1;
			dump_asts   = 1;
		} else if (strcmp(arg, "--dump-ast") == 0) {
			dump_asts = 1;
		} else if (strcmp(arg, "--dump-graph") == 0) {
			dump_graphs = 1;
		} else if (strcmp(arg, "--help") == 0) {
			usage(argv[0]);
			return 0;
		} else if (strcmp(arg, "-S") == 0) {
			mode = Compile;
		} else if (strcmp(arg, "-c") == 0) {
			mode = CompileAndLink;
		} else if (strcmp(arg, "-v") == 0) {
			verbose = 1;
		} else if (strncmp(arg, "-b", 2) == 0) {
			const char *bearg = arg+2;
			if (bearg[0] == 0) {
				++i;
				if (i >= argc) {
					usage(argv[0]);
					return 1;
				}
				bearg = argv[i];
			}
			if (!be_parse_arg(bearg)) {
				fprintf(stderr, "Invalid backend option: %s\n", bearg);
				usage(argv[0]);
				return 1;
			}
			if (strcmp(bearg, "help") == 0) {
				return 1;
			}
		} else if (arg[0] == '-') {
			fprintf(stderr, "Invalid option '%s'\n", arg);
			return 1;
		} else {
			const char *filename = argv[i];
			FILE *in;
			if (strcmp(filename, "-") == 0) {
				in       = stdin;
				/* nitpicking: is there a way so we can't have a normal file
				 * with the same name? probably not... */
				filename = "<stdin>";
			} else {
				in = fopen(filename, "r");
				if (in == NULL) {
					fprintf(stderr, "Couldn't open file '%s' for reading: %s\n",
					        filename, strerror(errno));
					exit(1);
				}
			}
			do_parse_file(in, filename);
			parsed++;
			if (in != stdin) {
				fclose(in);
			}
		}
	}
	if (parsed == 0) {
		fprintf(stderr, "Error: no input files specified\n");
		return 0;
	}
	if (had_parse_errors) {
		return 1;
	}

	gen_firm_init();

	do_check_semantic();

	ast2firm(modules);

	const char *asmname;
	if (mode == Compile) {
		asmname = outname;
	} else {
		asmname = TMPDIR "fluffy.s";
	}
	FILE* asm_out = fopen(asmname, "w");
	if (asm_out == NULL) {
		fprintf(stderr, "Couldn't open output '%s'\n", asmname);
		return 1;
	}
	gen_firm_finish(asm_out, asmname);
	fclose(asm_out);

	if (mode == CompileAndLink) {
		link(asmname, outname);
	}

	exit_mangle();
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

