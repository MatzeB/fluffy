CFLAGS=-Wall -W -Werror -O0 -g3 -I. -DHAVE_CONFIG_H 
LFLAGS=
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
