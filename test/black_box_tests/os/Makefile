TARGETS = dispatcher.o random.o ec.o pal.o tc.o

all: $(TARGETS)

clean:
	rm -f $(TARGETS)

%.o: %.cc
	g++ -c $<

%.o: %.c
	gcc -c $<
