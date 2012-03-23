.PHONY: all contiki clean test

include $(ROOT)/applications/contiki/config.mak

ifndef CSC
$(error please specify the simulation file)
endif

ifndef APP
$(error please specify your app)
endif

VERIFY = $(ROOT)/applications/contiki/$(APP)/verify.py
SKYS = $(shell perl -ne 'if (/<firmware[^>]*>\[CONFIG_DIR\]\/([^\.][^<]*)<\/firmware>/) { print "$$1 "; }' < $(CSC))

ifeq ($(TYPE), native)
all: contiki $(SKYS)

else ifeq ($(TYPE), generated)
ifndef XGCC
$(error please specify your cross compiler command line)
endif

export OCRAM_PAL_TEMPLATE = $(ROOT)/applications/contiki/tc/pal.jinja.c

all: app-ec.c pal.c contiki $(SKYS)

app-ec.c pal.c: app-tc.pped.c app-tc.o $(OCRAM) $(PALGEN) $(OCRAM_PAL_TEMPLATE)
	    $(OCRAM) -p pal.c -g $(PALGEN) -i $< -o app-ec.c || (rm -f app-ec.c pal.c; exit 1)

app-tc.pped.c: app-tc.c app-tc.o
	    $(CHROOT) $(XGCC) -I$(ROOT)/applications/contiki -E -DOCRAM_MODE $< -o $@

app-tc.o: app-tc.c
	    $(CHROOT) $(XGCC) -I$(ROOT)/applications/contiki -c $<

else ifeq ($(TYPE), runtime)
ifndef XGCC
$(error please specify your cross compiler command line)
endif

export THREAD_LIBRARY = $(ROOT)/applications/contiki/tl/pal.c

all: pal.c contiki $(SKYS)

pal.c: $(THREAD_LIBRARY)
	cp $< $@

else
$(error unknown application type)
endif

include $(ROOT)/applications/contiki/goals.mak

test: all OcramCooja.log

OcramCooja.log: $(TEST) $(VERIFY) $(CSC) $(SKYS)
	$(TEST) $(VERIFY) $(CSC) || (mv $@ failed$@; exit 1)
