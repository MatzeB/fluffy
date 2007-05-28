#ifndef SEMANTIC_T_H
#define SEMANTIC_T_H

#include "semantic.h"
#include "ast.h"
#include "type.h"
#include "adt/obst.h"
#include "symbol.h"
#include "lexer_t.h"

struct semantic_env_t {
	struct obstack       *obst;
	struct obstack        symbol_obstack;
	environment_entry_t **symbol_stack;
	struct obstack        label_obstack;
	symbol_t            **label_stack;
	int                   found_errors;

	method_t             *current_method;
	int                   last_statement_was_return;
	type_t               *type_bool;
	type_t               *type_byte;
	type_t               *type_int;
	type_t               *type_uint;
	type_t               *type_void_ptr;
	type_t               *type_byte_ptr;
};

void print_error_prefix(semantic_env_t *env, const source_position_t position);
void print_warning_prefix(semantic_env_t *env,
                          const source_position_t position);
void error_at(semantic_env_t *env, const source_position_t position,
              const char *message);

#endif
