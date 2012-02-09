include ../config.mk
-include depend.mak

export OCRAM_PAL_TEMPLATE = $(OS_ROOT)/os/pal.jinja.c

TARGETS=tc.elf ec.elf native.elf

ifeq ($(PLATFORM),linux)
LDFLAGS=-lpthread
endif

.PHONY: all clean test

all: $(TARGETS)

clean:
	rm -f *.elf *.asm *.pped.* ec.c *.o depend.mak

test: all
	./verify.sh

tc.elf: tc.o main-tc.o $(OS_ROOT)/os/libtc.a $(OS_ROOT)/os/libos.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

ec.elf: ec.o pal.o $(OS_ROOT)/os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

native.elf: native.o $(OS_ROOT)/os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

tc.pped.c: tc.c
	$(CC) $(CFLAGS) -DOCRAM_MODE -E $< -o $@

ec.c pal.c: tc.pped.c $(OCRAM) $(PALGEN) $(OCRAM_PAL_TEMPLATE)
	$(OCRAM) -i $< -o ec.c -g $(PALGEN) -p pal.c || (rm -f ec.c pal.c; exit 1)
