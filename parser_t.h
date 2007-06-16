#ifndef PARSER_T_H
#define PARSER_T_H

#include "parser.h"

#include <assert.h>

#include "token_t.h"
#include "lexer_t.h"

typedef expression_t* (*parse_expression_function)  (unsigned precedence);
typedef expression_t* (*parse_expression_infix_function) (unsigned precedence,
                                                          expression_t *left);
typedef statement_t*  (*parse_statement_function) ();
typedef namespace_entry_t*  (*parse_namespace_entry_function)
                            ();

typedef struct expression_parse_function_t {
	unsigned                         precedence;
	parse_expression_function        parser;
	unsigned                         infix_precedence;
	parse_expression_infix_function  infix_parser;
} expression_parse_function_t;

extern token_t token;
extern lexer_t lexer;

void register_expression_parser(parse_expression_function parser,
                                int token_type, unsigned precedence);

void register_expression_infix_parser(parse_expression_infix_function parser,
                                      int token_type, unsigned precedence);

void register_statement_parser(parse_statement_function parser, int token_type);

void register_namespace_parser(parse_namespace_entry_function parser,
                               int token_type);

expression_t *parse_sub_expression(unsigned precedence);

void parser_print_error_prefix();

expression_t      *parse_expression();
statement_t       *parse_statement();
namespace_entry_t *parse_namespace_entry();

#endif

