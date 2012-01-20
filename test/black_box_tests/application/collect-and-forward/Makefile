include ../../config.mk
-include depend.mak

.PHONY: all clean test

TARGETS=tc.elf ec.elf native.elf

ifeq ($(PLATFORM),linux)
LDFLAGS=-lpthread
endif

all: $(TARGETS)

clean:
	rm -f *.elf *.asm *.pped.* ec.c *.o depend.mak

test:
	./run-test.sh

OCRAM=../../../../cabal-dev/bin/ocram

tc.elf: tc.o main-tc.o ../../os/libtc.a ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

ec.elf: ec.o main-ec.o ../../os/libpal.a ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

native.elf: native.o ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

pal.pped.h: $(ROOT)/os/pal.h
	$(CC) -I$(ROOT) -DOCRAM_MODE -E $< -o $@

tc.pped.c: tc.c
	$(CC) -I$(ROOT) -DOCRAM_MODE -E $< -o $@

ec.c: tc.pped.c pal.pped.h $(OCRAM)
	$(OCRAM) -p pal.pped.h -i $< -o $@ || (rm -f $@; exit 1)
