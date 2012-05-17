#include <config.h>

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/time.h>

#include <libfirm/firm.h>
#include <libfirm/be.h>

#include "driver/firm_opt.h"
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

typedef struct file_list_entry_t file_list_entry_t;
struct file_list_entry_t {
	const char  *name; /**< filename or NULL for stdin */
	file_list_entry_t *next;
};

static file_list_entry_t *temp_files;

static machine_triple_t *target_machine;
static bool dump_graphs;
static bool dump_asts;
static bool verbose;
static bool had_parse_errors;

typedef enum compile_mode_t {
	Compile,
	CompileAndLink
} compile_mode_t;

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

static void do_link(const char *in, const char *out)
{
	char buf[4096];

	if (out == NULL) {
		out = "a.out";
	}

	int res = snprintf(buf, sizeof(buf), "%s -x assembler %s -o %s", LINKER, in, out);
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
	if (firm_is_unixish_os(target_machine)) {
		set_add_underscore_prefix(false);
	} else if (firm_is_darwin_os(target_machine)
	           || firm_is_windows_os(target_machine)) {
		set_add_underscore_prefix(true);
	} else {
		panic("Unsupported operating system ");
	}
}

static const char *try_dir(const char *dir)
{
	if (dir == NULL)
		return dir;
	if (access(dir, R_OK | W_OK | X_OK) == 0)
		return dir;
	return NULL;
}

static const char *get_tempdir(void)
{
	static const char *tmpdir = NULL;

	if (tmpdir != NULL)
		return tmpdir;

	if (tmpdir == NULL)
		tmpdir = try_dir(getenv("TMPDIR"));
	if (tmpdir == NULL)
		tmpdir = try_dir(getenv("TMP"));
	if (tmpdir == NULL)
		tmpdir = try_dir(getenv("TEMP"));

#ifdef P_tmpdir
	if (tmpdir == NULL)
		tmpdir = try_dir(P_tmpdir);
#endif

	if (tmpdir == NULL)
		tmpdir = try_dir("/var/tmp");
	if (tmpdir == NULL)
		tmpdir = try_dir("/usr/tmp");
	if (tmpdir == NULL)
		tmpdir = try_dir("/tmp");

	if (tmpdir == NULL)
		tmpdir = ".";

	return tmpdir;
}



/**
 * an own version of tmpnam, which: writes in a buffer, emits no warnings
 * during linking (like glibc/gnu ld do for tmpnam)...
 */
static FILE *make_temp_file(char *buffer, size_t buflen, const char *prefix)
{
	const char *tempdir = get_tempdir();

	snprintf(buffer, buflen, "%s/%sXXXXXX", tempdir, prefix);

	int fd = mkstemp(buffer);
	if (fd == -1) {
		fprintf(stderr, "could not create temporary file: %s\n",
		        strerror(errno));
		exit(EXIT_FAILURE);
	}
	FILE *out = fdopen(fd, "w");
	if (out == NULL) {
		fprintf(stderr, "could not create temporary file FILE*\n");
		exit(EXIT_FAILURE);
	}

	file_list_entry_t *entry = xmalloc(sizeof(*entry));
	memset(entry, 0, sizeof(*entry));

	size_t  name_len = strlen(buffer) + 1;
	char   *name     = malloc(name_len);
	memcpy(name, buffer, name_len);
	entry->name      = name;

	entry->next = temp_files;
	temp_files  = entry;

	return out;
}

static void free_temp_files(void)
{
	file_list_entry_t *entry = temp_files;
	file_list_entry_t *next;
	for ( ; entry != NULL; entry = next) {
		next = entry->next;

		unlink(entry->name);
		free((char*) entry->name);
		free(entry);
	}
	temp_files = NULL;
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
	gen_firm_init();
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

	do_check_semantic();

	ast2firm(modules);

	const char *asmname;
	char        temp[1024];
	if (mode == Compile) {
		asmname = outname;
	} else {
		FILE *tempf = make_temp_file(temp, sizeof(temp), "ccs");
		fclose(tempf);
		asmname = temp;
	}
	FILE* asm_out = fopen(asmname, "w");
	if (asm_out == NULL) {
		fprintf(stderr, "Couldn't open output '%s'\n", asmname);
		return 1;
	}
	generate_code(asm_out, asmname);
	fclose(asm_out);

	if (mode == CompileAndLink) {
		do_link(asmname, outname);
	}

	//free_temp_files();
	(void)free_temp_files;

	gen_firm_finish();
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

