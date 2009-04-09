LFLAGS = 
SOURCES = $(wildcard plugin_*.fluffy)
PLUGINS = $(addsuffix .dylib, $(basename $(SOURCES)))
FLUFFY_FLAGS = -bomitfp=no -bdebuginfo=stabs
DYNAMIC_LINK = -undefined dynamic_lookup

.PHONY: all clean

all: $(PLUGINS)

%.dylib: %.o
	#gcc -shared $*.s $(LFLAGS) -o $*.so
	libtool -dynamic -o $@ $(DYNAMIC_LINK) $*.o

%.o: %.s
	gcc -c $< -o $@

%.s: %.fluffy api.fluffy
	../fluffy --time $(FLUFFY_FLAGS) -S api.fluffy $*.fluffy -o $*.s

plugin_sql.s: plugin_while.dylib

plugin_enum.s: plugin_while.dylib

clean:
	rm -rf $(PLUGINS)
