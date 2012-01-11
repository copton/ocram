include ../../config.mk
-include depend.mak

.PHONY: all clean test

TARGETS=tc.elf ec.elf native.elf

ifeq ($(PLATFORM),linux)
LDFLAGS=-lpthread
endif

all: $(TARGETS)

clean:
	rm -f *.elf *.asm *.stripped ec.c *.o depend.mak

test:
	./run-test.sh

OCRAM=../../../../cabal-dev/bin/ocram

tc.elf: tc.o main-tc.o ../../os/libtc.a ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

ec.elf: ec.o main-ec.o ../../os/libpal.a ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

native.elf: native.o ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

ec.c: tc.c $(OCRAM) ../../os/pal.h
	$(OCRAM) -t "$(TOOLCHAIN)" -c "-I$(ROOT) -DOCRAM_MODE" -p $(ROOT)/os/pal.h -i $< -o $@ || (rm -f $@; exit 1)

