#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>
#include "ast.h"

namespace_t *parse(FILE *in, const char *input_name);

#endif
