#ifndef PARSE_EXPRESSION_H
#define PARSE_EXPRESSION_H

#include "parser.h"

typedef expression_t* (*parse_expression_function)
                      (parser_env_t *env, unsigned precedence);
typedef expression_t* (*parse_expression_infix_function)
                      (parser_env_t *env, unsigned precedence,
                       expression_t *left);

void register_expression_parser(parser_env_t *env,
                                parse_expression_function parser,
                                int token_type, unsigned precedence);

void register_expression_infix_parser(parser_env_t *env,
                                      parse_expression_infix_function parser,
                                      int token_type, unsigned precedence);

expression_t *parse_sub_expression(parser_env_t *env, unsigned precedence);

#endif

