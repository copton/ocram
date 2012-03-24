.PHONY: all contiki clean test

CHROOT = schroot -c tinyos -p --
XGCC = avr-gcc
export OCRAM_PAL_TEMPLATE = $(ROOT)/applications/tinyos/pal.jinja.c

all: app-ec.c pal.c tinyos

app-ec.c: app-tc.pped.c $(OCRAM) $(PALGEN) $(OCRAM_PAL_TEMPLATE)

app-tc.pped.c: app-tc.c app-tc.o
	$(CHROOT) $(XGCC) -E -DOCRAM_MODE @< -o $@

app-tc.o: app-tc.c
	$(CHROOT) $(XGCC) -c $<

tinyos:
	$(CHROOT) $(MAKE) -f Makefile.tinyos micaz sim

clean:
	$(CHROOT) $(MAKE) -f Makefile.tinyos clean

test: all
	$(CHROOT) ./test.py
