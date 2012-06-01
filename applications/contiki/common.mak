.PHONY: all contiki clean test

include $(ROOT)/applications/contiki/config.mak

ifndef APP
$(error please specify your app)
endif

OCRAM_CPP = $(ROOT)/applications/contiki/cpp
PALGEN = $(ROOT)/applications/contiki/tc/pal.py
VERIFY = $(ROOT)/applications/contiki/$(APP)/verify.py
export OCRAM_PAL_TEMPLATE = $(ROOT)/applications/contiki/tc/pal.jinja.c
CSC = simulation.csc
SKYS = $(shell perl -ne 'if (/<firmware[^>]*>\[CONFIG_DIR\]\/([^\.][^<]*)<\/firmware>/) { print "$$1 "; }' < $(CSC))

ifeq ($(TYPE), native)
all: contiki $(SKYS)

else ifeq ($(TYPE), generated)
ifndef XGCC
$(error please specify your cross compiler command line)
endif

all: app-ec.c pal.c contiki debug.json $(SKYS)

app-ec.c pal.c debug.json: app-tc.c app-tc.o $(OCRAM) $(PALGEN) $(OCRAM_PAL_TEMPLATE)
	$(OCRAM) -p pal.c  -i $< -o app-ec.c -d debug.json \
		-g $(PALGEN) \
		-t app-ptc.c \
		-c "$(CHROOT) $(XGCC) -DOCRAM_MODE -E -o -" \
		|| (rm -f app-ec.c pal.c; exit 1)

app-tc.o: app-tc.c
	$(CHROOT) $(XGCC) -Wall -c $<

else ifeq ($(TYPE), runtime)

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
