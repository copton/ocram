.PHONY: all clean

OCRAM = /home/alex/scm/ocram/cabal-dev/bin/ocram
PALGEN = ../../../os/pal.py
export OCRAM_PAL_TEMPLATE = /home/alex/scm/ocram/applications/contiki/os/pal.c

XGCC=schroot -c contiki -- msp430-gcc -DUIP_CONF_IPV6_RPL -DCONTIKI=1 -DCONTIKI_TARGET_SKY=1 -DUIP_CONF_IPV6=1 -DUIP_CONF_IPV6_RPL=1 -DWITH_UIP6=1 -Os -fno-strict-aliasing -ffunction-sections -Wall -mmcu=msp430x1611 -g   -I. -I/home/alex/scm/contiki//platform/sky/. -I/home/alex/scm/contiki//platform/sky/dev -I/home/alex/scm/contiki//platform/sky/apps -I/home/alex/scm/contiki//platform/sky/net -I/home/alex/scm/contiki//cpu/msp430/f1xxx -I/home/alex/scm/contiki//cpu/msp430/. -I/home/alex/scm/contiki//cpu/msp430/dev -I/home/alex/scm/contiki//core/dev -I/home/alex/scm/contiki//core/lib -I/home/alex/scm/contiki//core/net -I/home/alex/scm/contiki//core/net/mac -I/home/alex/scm/contiki//core/net/rime -I/home/alex/scm/contiki//core/net/rpl -I/home/alex/scm/contiki//core/sys -I/home/alex/scm/contiki//core/cfs -I/home/alex/scm/contiki//core/ctk -I/home/alex/scm/contiki//core/lib/ctk -I/home/alex/scm/contiki//core/loader -I/home/alex/scm/contiki//core/.

all: app-ec
	
app-ec: app-ec.c pal.c
	schroot -c contiki -- $(MAKE) -f Makefile.contiki

app-ec.c pal.c: app-tc.pped.c app-tc.o $(OCRAM) $(PALGEN) $(OCRAM_PAL_TEMPLATE)
	$(OCRAM) -p pal.c -g $(PALGEN) -i $< -o app-ec.c || (rm -f app-ec.c pal.c; exit 1)

app-tc.pped.c: app-tc.c app-tc.o
	$(XGCC) -E -DOCRAM_MODE $< -o $@

app-tc.o: app-tc.c
	$(XGCC) -c $<

clean:
	schroot -c contiki -- make -f Makefile.contiki clean
	rm -f app-ec.c pal.h app-tc.pped.c app-tc.o pal.c
