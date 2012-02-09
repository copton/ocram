.PHONY: all contiki clean test

ifndef ROOT
$(error no ROOT defined)
endif

ifndef CSC
$(error please specify the simulation file)
endif

ifndef APP
$(error please specify your app)
endif

TEST = $(ROOT)/applications/contiki/test.py
VERIFY = $(ROOT)/applications/contiki/$(APP)/verify.py
CACHE = $(ROOT)/applications/contiki/cache.py
SKYS = $(shell perl -ne 'if (/<firmware[^>]*>\[CONFIG_DIR\]\/([^\.][^<]*)<\/firmware>/) { print "$$1 "; }' < $(CSC))

ifeq ($(TYPE), native)
all: contiki $(SKYS)

else ifeq ($(TYPE), generated)
ifndef XGCC
$(error please specify your cross compiler command line)
endif

export OCRAM_PAL_TEMPLATE = $(ROOT)/applications/contiki/os/pal.jinja.c

all: app-ec.c pal.c contiki $(SKYS)

app-ec.c pal.c: app-tc.pped.c app-tc.o $(OCRAM) $(PALGEN) $(OCRAM_PAL_TEMPLATE)
	    $(OCRAM) -p pal.c -g $(PALGEN) -i $< -o app-ec.c || (rm -f app-ec.c pal.c; exit 1)

app-tc.pped.c: app-tc.c app-tc.o
	    $(XGCC) -I$(ROOT)/applications/contiki -E -DOCRAM_MODE $< -o $@

app-tc.o: app-tc.c
	    $(XGCC) -I$(ROOT)/applications/contiki -c $<

else
$(error unknown application type)
endif

$(SKYS): %.cached.sky: $(CACHE) %.sky
	$^

contiki:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki

clean:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki clean
	git clean -d -x -f

test: all OcramCooja.log

OcramCooja.log: $(TEST) $(VERIFY) $(CSC) $(SKYS)
	$(TEST) $(VERIFY) $(CSC)
