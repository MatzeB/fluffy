LFLAGS = 
SOURCES = $(wildcard plugin_*.fluffy)
PLUGINS = $(addsuffix .so, $(basename $(SOURCES)))
FLUFFY_FLAGS = -bomitfp=no -bstabs=on

.PHONY: all clean

all: $(PLUGINS)

%.so: %.s
	gcc -shared $*.s $(LFLAGS) -o $*.so

%.s: %.fluffy api.fluffy
	../fluffy $(FLUFFY_FLAGS) -S api.fluffy $*.fluffy -o $*.s

plugin_sql.s: plugin_while.so

plugin_enum.s: plugin_while.so

clean:
	rm -rf $(PLUGINS)
