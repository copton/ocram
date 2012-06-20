include ../config.mk
-include depend.mak

BENCH=$(ROOT)/applications/simulation_os/bench.py
PALGEN=$(ROOT)/applications/pal.py

export OCRAM_PAL_TEMPLATE = $(OS_ROOT)/os/pal.jinja.c

TARGETS=tc.elf ec.elf native.elf

ifeq ($(PLATFORM),linux)
LDFLAGS=-lpthread
endif

.PHONY: all clean test bench

all: $(TARGETS)

clean:
	git clean -d -f -x

ifeq ($(PLATFORM),linux)
test: all
	./verify.sh

else
test: all

endif

bench: test $(BENCH)
	$(BENCH) `pwd`

tc.elf: tc.o main-tc.o $(OS_ROOT)/os/libtc.a $(OS_ROOT)/os/libos.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

ec.elf: ec.o pal.o $(OS_ROOT)/os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

native.elf: native.o $(OS_ROOT)/os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

tc.pped.c: tc.c
	$(CC) $(CFLAGS) -DOCRAM_MODE -E $< -o $@

ec.c pal.c: tc.pped.c $(OCRAM) $(PALGEN) $(OCRAM_PAL_TEMPLATE)
	$(OCRAM) --compile -p pal.c -i $< -o $@ -g $(PALGEN) -c "$(CC) $(CFLAGS) -DOCRAM_MODE -E -o -" || (rm -f ec.c pal.c; exit 1)
