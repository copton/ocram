.PHONY: all contiki clean test

CHROOT = schroot -c tinyos -p --
XGCC = /home/alex/code/permasense/tools/msp430Xgcc/bin/msp430-gcc
NESCC = /opt/local/bin/nescc

export OCRAM_PAL_TEMPLATE = $(ROOT)/applications/tinyos/pal.jinja.c

all: app-ec.c pal.c tinyos

app-ec.c pal.c: app-tc.pped.c $(OCRAM) $(PALGEN) $(OCRAM_PAL_TEMPLATE)
	$(OCRAM) -p pal.c -g $(PALGEN) -i $< -o app-ec.c || (rm -f app-ec.c pal.c; exit 1)

app-tc.pped.c: app-tc.c app-tc.o
	$(CHROOT) $(XGCC) -E -DOCRAM_MODE -E $< -o $@

app-tc.o: app-tc.c
	$(CHROOT) $(XGCC) -c $<

tinyos:
	$(CHROOT) $(MAKE) -f Makefile.tinyos micaz sim

clean:
	$(CHROOT) $(MAKE) -f Makefile.tinyos clean
	git clean -d -x -f

test: all
	$(CHROOT) ./test.py
