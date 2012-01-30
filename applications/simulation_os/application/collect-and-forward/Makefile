include ../../config.mk
-include depend.mak

.PHONY: all clean test

OCRAM=../../../../cabal-dev/bin/ocram
PALGEN=../../../pal.py
export OCRAM_PAL_TEMPLATE = $(ROOT)/os/pal.c

TARGETS=tc.elf ec.elf native.elf

ifeq ($(PLATFORM),linux)
LDFLAGS=-lpthread
endif

all: $(TARGETS)

clean:
	rm -f *.elf *.asm *.pped.* ec.c *.o depend.mak

test:
	./run-test.sh


tc.elf: tc.o main-tc.o ../../os/libtc.a ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

ec.elf: ec.o pal.o ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

native.elf: native.o ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

tc.pped.c: tc.c
	$(CC) -I$(ROOT) -DOCRAM_MODE -E $< -o $@

ec.c pal.c: tc.pped.c $(OCRAM) $(PALGEN)
	$(OCRAM) -i $< -o ec.c -g $(PALGEN) -p pal.c || (rm -f ec.c pal.c; exit 1)
