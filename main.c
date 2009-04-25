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

#include "type.h"
#include "parser.h"
#include "ast_t.h"
#include "semantic.h"
#include "ast2firm.h"
#include "plugins.h"
#include "type_hash.h"
#include "mangle.h"
#include "adt/error.h"

#ifdef _WIN32
	#define LINKER "gcc.exe"
	#define TMPDIR ""
	#define DEFAULT_OS  TARGET_OS_MINGW
#else
	#if defined(__APPLE__)
		#define DEFAULT_OS  TARGET_OS_MACHO
	#else
		#define DEFAULT_OS  TARGET_OS_ELF
	#endif
	#define LINKER "gcc"
	#define TMPDIR "/tmp/"
#endif

typedef enum {
	TARGET_OS_MINGW,
	TARGET_OS_ELF,
	TARGET_OS_MACHO
} target_os_t;

static int dump_graphs = 0;
static int dump_asts   = 0;
static int verbose     = 0;
static int show_timers = 0;
static int noopt       = 0;
static int do_inline   = 1;
static target_os_t target_os = DEFAULT_OS;

typedef enum compile_mode_t {
	Compile,
	CompileAndLink
} compile_mode_t;

const ir_settings_if_conv_t *if_conv_info = NULL;

static void set_be_option(const char *arg)
{
	int res = be_parse_arg(arg);
	(void) res;
	assert(res);
}

static void initialize_firm(void)
{
	be_opt_register();

	dbg_init(NULL, NULL, dbg_snprint);

	switch (target_os) {
	case TARGET_OS_MINGW:
		set_be_option("ia32-gasmode=mingw");
		break;
	case TARGET_OS_ELF:
		set_be_option("ia32-gasmode=elf");
		break;
	case TARGET_OS_MACHO:
		set_be_option("ia32-gasmode=macho");
		set_be_option("ia32-stackalign=4");
		set_be_option("pic");
		break;
	}
}

static void get_output_name(char *buf, size_t buflen, const char *inputname,
                     const char *newext)
{
	size_t last_dot = 0xffffffff;
	size_t i = 0;
	for(const char *c = inputname; *c != 0; ++c) {
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

static void dump_ast(const namespace_t *namespace, const char *name)
{
	if (!dump_asts)
		return;

	const char *fname = namespace->filename;
	char        filename[4096];
	get_output_name(filename, sizeof(filename), fname, name);

	FILE* out = fopen(filename, "w");
	if (out == NULL) {
		fprintf(stderr, "Warning: couldn't open '%s': %s\n", filename,
		        strerror(errno));
	} else {
		print_ast(out, namespace);
	}
	fclose(out);
}

static void parse_file(const char *fname)
{
	FILE *in = fopen(fname, "r");
	if (in == NULL) {
		fprintf(stderr, "couldn't open '%s' for reading: %s\n", fname,
		        strerror(errno));
		exit(1);
	}

	namespace_t *namespace = parse(in, fname);
	fclose(in);

	if (namespace == NULL) {
		exit(1);
	}
	dump_ast(namespace, "-parse.txt");
}

static void check_semantic(void)
{
	if (!check_static_semantic()) {
		fprintf(stderr, "Semantic errors found\n");
		namespace_t *namespace = namespaces;
		while (namespace != NULL) {
			dump_ast(namespace, "-error.txt");
			namespace = namespace->next;
		}
		exit(1);
	}

	if (dump_asts) {
		namespace_t *namespace = namespaces;
		while (namespace != NULL) {
			dump_ast(namespace, "-semantic.txt");
			namespace = namespace->next;
		}
	}
}

static void link(const char *in, const char *out)
{
	char buf[4096];

	if (out == NULL) {
		out = "a.out";
	}

	snprintf(buf, sizeof(buf), "%s %s -o %s", LINKER, in, out);
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

void lower_compound_params(void)
{
}

int main(int argc, const char **argv)
{
	gen_firm_init();
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

	firm_opt.lower_ll = false;

	const char *outname = NULL;
	compile_mode_t mode = CompileAndLink;
	int parsed = 0;

	for(int i = 1; i < argc; ++i) {
		const char *arg = argv[i];
		if (strcmp(arg, "-o") == 0) {
			++i;
			if (i >= argc) {
				usage(argv[0]);
				return 1;
			}
			outname = argv[i];
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
		} else if (strcmp(arg, "--time") == 0) {
			show_timers = 1;
		} else if (arg[0] == '-' && arg[1] == 'O') {
			int optlevel = atoi(&arg[2]);
			if (optlevel <= 0) {
				noopt = 1;
			} else if (optlevel > 1) {
				do_inline = 1;
			} else {
				noopt     = 0;
				do_inline = 0;
			}
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
		} else {
			parsed++;
			parse_file(argv[i]);
		}
	}
	if (parsed == 0) {
		fprintf(stderr, "Error: no input files specified\n");
		return 0;
	}

	check_semantic();

	ast2firm();

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
	gen_firm_finish(asm_out, asmname, 1, true);
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

