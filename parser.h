#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>
#include "ast.h"

namespace_t *parse(FILE *in, const char *input_name);

void init_parser(void);
void exit_parser(void);

#endif
