GOAL = mlang

CFLAGS += -Wall -W -Werror -O0 -g3
CFLAGS += -DHAVE_CONFIG_H
CFLAGS += -I .
CFLAGS += -I include
CFLAGS += -I include/firm/ana
CFLAGS += -I include/firm/ana2
CFLAGS += -I include/firm/arch
CFLAGS += -I include/firm/common
CFLAGS += -I include/firm/debug
CFLAGS += -I include/firm/ident
CFLAGS += -I include/firm/ir
CFLAGS += -I include/firm/lower
CFLAGS += -I include/firm/opt
CFLAGS += -I include/firm/stat
CFLAGS += -I include/firm/tr
CFLAGS += -I include/firm/tv

LFLAGS = -Llib -lfirm -lcore -llpp -lm
ifeq ($(OSTYPE), FreeBSD)
LFLAGS += -lobstack
else
LFLAGS += -ldl
endif

SOURCES :=
SOURCES += adt/hashset.c
SOURCES += adt/pset.c
SOURCES += adt/pset_new.c
SOURCES += adt/strset.c
SOURCES += adt/xmalloc.c
SOURCES += ast.c
SOURCES += ast2firm.c
SOURCES += known_symbols.c
SOURCES += lexer.c
SOURCES += main.c
SOURCES += parser.c
SOURCES += semantic.c
SOURCES += symbol_table.c
SOURCES += token.c
SOURCES += type_hash.c

OBJECTS = $(SOURCES:%.c=build/%.o)

Q = @

.PHONY : all clean dirs

all: $(GOAL)

ifeq ($(findstring $(MAKECMDGOALS), clean depend),)
-include .depend
endif

.depend: $(SOURCES)
	@echo "===> DEPEND"
	@rm -f $@ && touch $@ && makedepend -p "$@ build/" -Y -f $@ -- $(CFLAGS) -- $(SOURCES) 2> /dev/null && rm $@.bak

$(GOAL): build/adt $(OBJECTS)
	@echo "===> LD $@"
	$(Q)$(CC) $(OBJECTS) $(LFLAGS) -o $(GOAL)

build/adt:
	@echo "===> MKDIR $@"
	$(Q)mkdir -p $@

build/%.o: %.c
	@echo '===> CC $<'
	$(Q)$(CC) $(CFLAGS) -c $< -o $@

clean:
	@echo '===> CLEAN'
	$(Q)rm -rf build $(GOAL) .depend
