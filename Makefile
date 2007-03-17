CFLAGS=-Wall -W -Werror -O2 -g3 -I.
LFLAGS=
GOAL=mlang
SOURCES=$(wildcard *.c) $(wildcard adt/*.c)
OBJECTS=$(addsuffix .o, $(basename $(SOURCES)))

$(GOAL): $(OBJECTS)
	gcc -o $(GOAL) $(OBJECTS) $(LFLAGS)

%.o: %.c
	gcc -c $(CFLAGS) -o $*.o $*.c

clean:
	rm -rf $(OBJECTS) $(GOAL)
