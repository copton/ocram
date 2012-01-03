include ../../config.mk
-include depend.mak

.PHONY: all clean test

MAJOR_TARGETS=tc.exe ec.exe native.exe
ifndef DEBUG
DEBUG_TARGETS=$(patsubst %.exe, %.stripped, $(MAJOR_TARGETS))
endif
ifdef CROSS
CROSS_TARGETS=$(patsubst %.exe, %.asm, $(MAJOR_TARGETS))
endif

ifeq ($(PLATFORM),native)
LDFLAGS=-lpthread
endif

all: $(MAJOR_TARGETS) $(DEBUG_TARGETS) $(CROSS_TARGETS)

clean:
	rm -f *.exe *.asm *.stripped ec.c *.o depend.mak

test:
	./run-test.sh

OCRAM=../../../../cabal-dev/bin/ocram

tc.exe: tc.o main-tc.o ../../os/libtc.a ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^ $(LDFLAGS)

ec.exe: ec.o main-ec.o ../../os/libpal.a ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^

native.exe: native.o ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^

ec.c: tc.c $(OCRAM) ../../os/pal.h
	$(OCRAM) -t "$(TOOLCHAIN)" -c "-I$(ROOT) -DOCRAM_MODE" -p $(ROOT)/os/pal.h -i $< -o $@ || (rm -f $@; exit 1)

%.asm: %.exe
	$(OBJDUMP) -zhD $< > $@

%.stripped: %.exe
	$(STRIP) -o $@ $<
