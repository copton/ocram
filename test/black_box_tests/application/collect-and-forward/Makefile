include ../../config.mk

.PHONY: all clean test

all: tc.exe ec.exe native.exe

clean:
	rm -f *.exe ec.c *.o

test:
	./run-test.sh

OCRAM=../../../../cabal-dev/bin/ocram

tc.exe: tc.o main-tc.o ../../os/libtc.a ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^ -lpthread

ec.exe: ec.o main-ec.o ../../os/libpal.a ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^

native.exe: native.o ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^

ec.c: tc.c $(OCRAM) ../../os/pal.h
	$(OCRAM) -c "-I../.. -DOCRAM_MODE" -p ../../os/pal.h -i $< -o $@ || (rm -f $@; exit 1)

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I../../
