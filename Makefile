GOAL = mlang

CFLAGS += -Wall -W -Werror -O0 -g3 -std=c99
CFLAGS += -DHAVE_CONFIG_H
CFLAGS += -I .
CFLAGS += `pkg-config --cflags libfirm`

LFLAGS = `pkg-config --libs libfirm`

SOURCES := \
	adt/hashset.c \
	adt/pset.c \
	adt/strset.c \
	adt/xmalloc.c \
	ast.c \
	ast2firm.c \
	lexer.c \
	main.c \
	mangle_type.c \
	match_type.c \
	parser.c \
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
