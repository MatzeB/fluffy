#ifndef PLUGINS_T_H
#define PLUGINS_T_H

#include "plugins.h"

typedef void (*init_plugin_function)(void);

typedef struct plugin_t plugin_t;
struct plugin_t {
	init_plugin_function  init_function;
	void                 *dlhandle;
	plugin_t             *next;
};

extern plugin_t *plugins;

#endif

