#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>
#include "ast.h"

typedef struct parser_env_t parser_env_t;

namespace_t *parse(FILE *in, const char *input_name);

void init_parser(void);
void exit_parser(void);

#endif
