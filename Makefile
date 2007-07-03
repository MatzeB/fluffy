GOAL = fluffy

#FIRM_CFLAGS = `pkg-config --cflags libfirm`
#FIRM_LIBS = `pkg-config --libs libfirm`
FIRM_HOME = $(HOME)/projects/firm/
FIRM_BUILD = $(FIRM_HOME)/build/i686-pc-linux-gnu/debug/
FIRM_CFLAGS = -I$(FIRM_HOME)/libfirm/include -I$(HOME)/projects/firm/obstack -I$(HOME)/projects/firm/libcore
FIRM_LIBS = -L$(FIRM_BUILD) -lfirm -llpp -lcore -lm -ldl

CFLAGS += -Wall -W -Werror -O0 -g3 -std=c99
CFLAGS += -DHAVE_CONFIG_H
CFLAGS += -I .
CFLAGS += $(FIRM_CFLAGS) $(ADDCFLAGS)

LFLAGS = $(FIRM_LIBS)

SOURCES := \
	adt/pset.c \
	adt/strset.c \
	ast.c \
	ast2firm.c \
	lexer.c \
	main.c \
	mangle_type.c \
	match_type.c \
	parser.c \
	plugins.c \
	semantic.c \
	symbol_table.c \
	token.c \
	type.c \
	type_hash.c

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
	$(Q)$(CC) -rdynamic $(OBJECTS) $(LFLAGS) -o $(GOAL)

build/adt:
	@echo "===> MKDIR $@"
	$(Q)mkdir -p $@

build/%.o: %.c
	@echo '===> CC $<'
	$(Q)$(CC) $(CFLAGS) -c $< -o $@

clean:
	@echo '===> CLEAN'
	$(Q)rm -rf build $(GOAL) .depend
