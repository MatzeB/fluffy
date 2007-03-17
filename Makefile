CFLAGS=-Wall -W -Werror -O3 -I. -DHAVE_CONFIG_H -march=pentium3 -DNDEBUG
LFLAGS=
GOAL=mlang
FGOAL=ftest
SOURCES=$(wildcard *.c) $(wildcard adt/*.c)
FSOURCES=$(wildcard firm/*.c)
OBJECTS=$(addsuffix .o, $(basename $(SOURCES)))
FOBJECTS=$(addsuffix .o, $(basename $(FSOURCES)))

all: $(GOAL) $(FGOAL)

$(GOAL): $(OBJECTS)
	gcc -o $(GOAL) $(OBJECTS) $(LFLAGS)

$(FGOAL): $(FOBJECTS)
	gcc -o $(FGOAL) $(FOBJECTS) $(LFLAGS)

%.o: %.c
	gcc -c $(CFLAGS) -o $*.o $*.c

clean:
	rm -rf $(OBJECTS) $(GOAL) $(FOBJECTS) $(FGOAL)
