include ../../config.mk

.PHONY: all clean

all: tc.elf ec.elf native.elf

clean:
	rm -f *.elf ec.c *.o

OCRAM=../../../../cabal-dev/bin/ocram

tc.elf: tc.o main-tc.o ../../os/libtc.a ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^ -lpthread

ec.elf: ec.o main-ec.o ../../os/libpal.a ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^

native.elf: native.o ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^

ec.c: tc.c $(OCRAM) ../../os/pal.h
	$(OCRAM) -c "-I../.. -DOCRAM_MODE" -p ../../os/pal.h -i $< -o $@ || (rm -f $@; exit 1)

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I../../
