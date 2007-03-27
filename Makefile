CFLAGS=-Wall -W -Werror -O0 -g3 -I. -DHAVE_CONFIG_H -Iinclude -Iinclude/firm/ir -Iinclude/firm/tr -Iinclude/firm/tv -Iinclude/firm/ident -Iinclude/firm/common -Iinclude/firm/ana -Iinclude/firm/ana2 -Iinclude/firm/debug -Iinclude/firm/opt -Iinclude/firm/lower -Iinclude/firm/arch -Iinclude/firm/stat
LFLAGS=-Llib -lfirm -lcore -llpp -lm -ldl
GOAL=mlang
FGOAL=ftest
SOURCES=$(wildcard *.c) $(wildcard adt/*.c)
OBJECTS=$(addsuffix .o, $(basename $(SOURCES)))

all: $(GOAL)

$(GOAL): $(OBJECTS)
	gcc -o $(GOAL) $(OBJECTS) $(LFLAGS)

%.o: %.c
	gcc -c $(CFLAGS) -o $*.o $*.c

clean:
	rm -rf $(OBJECTS) $(GOAL)
