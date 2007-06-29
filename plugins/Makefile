LFLAGS = 
SOURCES = $(wildcard plugin_*.fluffy)
PLUGINS = $(addsuffix .so, $(basename $(SOURCES)))

.PHONY: all clean

all: $(PLUGINS)

%.so: %.s
	gcc -shared $*.s $(LFLAGS) -o $*.so

%.s: %.fluffy api.fluffy
	../fluffy api.fluffy $*.fluffy -o $*.s

plugin_sql.s: plugin_while.so

clean:
	rm -rf $(PLUGINS)
