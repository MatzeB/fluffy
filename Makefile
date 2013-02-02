-include config.mak

GOAL = fluffy

FIRM_CFLAGS ?= `pkg-config --cflags libfirm`
FIRM_LIBS ?= `pkg-config --libs libfirm`

CPPFLAGS = -I.
CPPFLAGS += $(FIRM_CFLAGS) -DFIRM_BACKEND

CFLAGS += -Wall -W -Wextra -Wstrict-prototypes -Wwrite-strings -Wmissing-prototypes -Werror -std=c99
CFLAGS += -O0 -g3

LFLAGS += $(FIRM_LIBS) -ldl

SOURCES := \
	adt/obstack.c \
	adt/obstack_printf.c \
	adt/strset.c \
	ast.c \
	ast2firm.c \
	driver/firm_machine.c \
	driver/firm_opt.c \
	driver/firm_timing.c \
	input.c \
	lexer.c \
	main.c \
	mangle.c \
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
	@rm -f $@ && touch $@ && makedepend -p "$@ build/" -Y -f $@ -- $(CPPFLAGS) -- $(SOURCES) 2> /dev/null && rm $@.bak

$(GOAL): $(OBJECTS) | build/driver build/adt
	@echo "===> LD $@"
	$(Q)$(CC) -rdynamic $(OBJECTS) $(LFLAGS) -o $(GOAL)

build/adt:
	@echo "===> MKDIR $@"
	$(Q)mkdir -p $@

build/driver:
	@echo "===> MKDIR $@"
	$(Q)mkdir -p $@

build/%.o: %.c | build/adt build/driver
	@echo '===> CC $<'
	$(Q)$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean:
	@echo '===> CLEAN'
	$(Q)rm -rf build $(GOAL) .depend
