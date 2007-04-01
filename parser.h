#ifndef _PARSER_H_
#define _PARSER_H_

#include <stdio.h>
#include "ast.h"

namespace_t *parse(FILE *in, const char *input_name);

#endif

