LFLAGS = 
SOURCES = $(wildcard plugin_*.ml)
PLUGINS = $(addsuffix .so, $(basename $(SOURCES)))

.PHONY: all clean

all: $(PLUGINS)

%.so: %.s
	gcc -shared $*.s $(LFLAGS) -o $*.so

%.s: %.ml api.ml
	../mlang api.ml $*.ml -o $*.s

clean:
	rm -rf $(PLUGINS)
