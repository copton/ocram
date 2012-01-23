.PHONY: all clean

OCRAM=/home/alex/scm/ocram/cabal-dev/bin/ocram

XGCC=schroot -c contiki -- msp430-gcc -DUIP_CONF_IPV6_RPL -DCONTIKI=1 -DCONTIKI_TARGET_SKY=1 -DUIP_CONF_IPV6=1 -DWITH_UIP6=1 -Wall -mmcu=msp430x1611  -I. -I/home/alex/scm/contiki//platform/sky/. -I/home/alex/scm/contiki//platform/sky/dev -I/home/alex/scm/contiki//platform/sky/apps -I/home/alex/scm/contiki//platform/sky/net -I/home/alex/scm/contiki//cpu/msp430/. -I/home/alex/scm/contiki//cpu/msp430/dev -I/home/alex/scm/contiki//core/dev -I/home/alex/scm/contiki//core/lib -I/home/alex/scm/contiki//core/net -I/home/alex/scm/contiki//core/net/mac -I/home/alex/scm/contiki//core/net/rime -I/home/alex/scm/contiki//core/net/rpl -I/home/alex/scm/contiki//core/sys -I/home/alex/scm/contiki//core/cfs -I/home/alex/scm/contiki//core/ctk -I/home/alex/scm/contiki//core/lib/ctk -I/home/alex/scm/contiki//core/loader -I/home/alex/scm/contiki//core/. -DAUTOSTART_ENABLE

all: app-ec
	
app-ec: app-ec.c pal.h
	schroot -c contiki -- $(MAKE) -f Makefile.contiki

app-ec.c: app-tc.pped.c app-tc.o
	$(OCRAM) -p pal.h -d -i $< -o $@

app-tc.pped.c: app-tc.c app-tc.o tc.h types.h
	$(XGCC) -E -DOCRAM_MODE $< -o $@

app-tc.o: app-tc.c
	$(XGCC) -c $<

clean:
	schroot -c contiki -- make -f Makefile.contiki clean
	rm -f app-ec.c pal.h app-tc.pped.c app-tc.o
