LFLAGS = 
SOURCES = $(wildcard plugin_*.fluffy)
PLUGINS = $(addsuffix .dylib, $(basename $(SOURCES)))
FLUFFY_FLAGS = -bomitfp=no -bdebuginfo=stabs
DYNAMIC_LINK = -undefined dynamic_lookup
STDLIB = ../stdlib/cstdio.fluffy ../stdlib/cstring.fluffy ../stdlib/cstdlib.fluffy ../stdlib/ctypes.fluffy ../stdlib/cassert.fluffy

.PHONY: all clean

all: $(PLUGINS)

%.dylib: %.o
	gcc -m32 -shared -undefined dynamic_lookup $*.o $(LFLAGS) -o $*.dylib
	#libtool -m32 -dynamic -o $@ $(DYNAMIC_LINK) $*.o

%.o: %.s
	gcc -m32 -c $< -o $@

%.s: %.fluffy api.fluffy $(STDLIB)
	../fluffy $(FLUFFY_FLAGS) $(STDLIB) -S api.fluffy $*.fluffy -o $*.s

plugin_sql.s: plugin_while.dylib

plugin_enum.s: plugin_while.dylib

clean:
	rm -rf $(PLUGINS)
