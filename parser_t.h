#ifndef PARSER_T_H
#define PARSER_T_H

#include "token_t.h"
#include "parser.h"

#include <assert.h>

#include "lexer.h"
#include "type.h"

typedef expression_t* (*parse_expression_function)       (void);
typedef expression_t* (*parse_expression_infix_function) (expression_t *left);
typedef statement_t*  (*parse_statement_function)       (void);
typedef void          (*parse_declaration_function)     (void);
typedef attribute_t*  (*parse_attribute_function)       (void);

typedef struct expression_parse_function_t {
	parse_expression_function        parser;
	unsigned                         infix_precedence;
	parse_expression_infix_function  infix_parser;
} expression_parse_function_t;

extern token_t token;

void register_expression_parser(parse_expression_function parser,
                                int token_type);

void register_expression_infix_parser(parse_expression_infix_function parser,
                                      int token_type, unsigned precedence);

void register_statement_parser(parse_statement_function parser, int token_type);

void register_declaration_parser(parse_declaration_function parser,
                                 int token_type);

void register_attribute_parser(parse_attribute_function parser, int token_type);

expression_t *parse_sub_expression(unsigned precedence);
void add_declaration(declaration_t *entry);

void parser_print_error_prefix(void);

expression_t      *parse_expression(void);
statement_t       *parse_statement(void);
type_t            *parse_type(void);
void               parse_declaration(void);
attribute_t       *parse_attributes(void);

void next_token(void);

#endif

