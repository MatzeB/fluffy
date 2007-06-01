#ifndef PARSER_T_H
#define PARSER_T_H

#include "parser.h"

#include <assert.h>

#include "token_t.h"
#include "lexer_t.h"

//#define ABORT_ON_ERROR
#define LOOKAHEAD	1
//#define PRINT_TOKENS

typedef struct lexer_state_t {
	token_t            token;
	source_position_t  source_position;
} lexer_state_t;

typedef expression_t* (*parse_expression_function)
                      (parser_env_t *env, unsigned precedence);
typedef expression_t* (*parse_expression_infix_function)
                      (parser_env_t *env, unsigned precedence,
                       expression_t *left);
typedef statement_t*  (*parse_statement_function) (parser_env_t *env);

typedef struct expression_parse_function_t {
	unsigned                         precedence;
	parse_expression_function        parser;
	unsigned                         infix_precedence;
	parse_expression_infix_function  infix_parser;
} expression_parse_function_t;

struct parser_env_t {
	token_t                      token;
	source_position_t            source_position;
	lexer_state_t                lookahead[LOOKAHEAD];
	expression_parse_function_t *expression_parsers;
	parse_statement_function    *statement_parsers;
	lexer_t                      lexer;
	symbol_table_t               symbol_table;
	int                          error;
	struct obstack               obst;
};

void register_expression_parser(parser_env_t *env,
                                parse_expression_function parser,
                                int token_type, unsigned precedence);

void register_expression_infix_parser(parser_env_t *env,
                                      parse_expression_infix_function parser,
                                      int token_type, unsigned precedence);

void register_statement_parser(parser_env_t *env,
                               parse_statement_function parser, int token_type);

expression_t *parse_sub_expression(parser_env_t *env, unsigned precedence);

static inline
void next_token(parser_env_t *env)
{
	env->token           = env->lookahead[0].token;
	env->source_position = env->lookahead[0].source_position;
#if LOOKAHEAD > 1
	memmove(env->lookahead, env->lookahead + 1,
	        (LOOKAHEAD - 1) * sizeof(env->lookahead[0]));
#endif
	lexer_state_t *state = &env->lookahead[LOOKAHEAD - 1];
	lexer_next_token(&env->lexer, & state->token);
	state->source_position = env->lexer.source_position;

#ifdef PRINT_TOKENS
	print_token(stderr, & env->token);
	fprintf(stderr, "\n");
#endif
}

static inline
void eat(parser_env_t *env, token_type_t type)
{
	assert(env->token.type == type);
	next_token(env);
}

/** look ahead some symbols */
static inline
const token_t* la(parser_env_t *env, int i)
{
	assert(i >= 1);
	assert(i <= LOOKAHEAD);
	return & env->lookahead[i - 1].token;
}

#endif

