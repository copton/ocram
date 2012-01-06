include ../../config.mk
-include depend.mak

.PHONY: all clean test

EXE_TARGETS=tc.exe ec.exe native.exe
OD_TARGETS=$(patsubst %.exe, %.od, $(EXE_TARGETS))

ifeq ($(PLATFORM),linux)
LDFLAGS=-lpthread
endif

all: $(EXE_TARGETS) $(OD_TARGETS)

clean:
	rm -f *.exe *.asm *.stripped ec.c *.o depend.mak

test:
	./run-test.sh

OCRAM=../../../../cabal-dev/bin/ocram

tc.exe: tc.o main-tc.o ../../os/libtc.a ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

ec.exe: ec.o main-ec.o ../../os/libpal.a ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

native.exe: native.o ../../os/libos.a
	$(CC) $(CFLAGS) -o $@ $^

ec.c: tc.c $(OCRAM) ../../os/pal.h
	$(OCRAM) -t "$(TOOLCHAIN)" -c "-I$(ROOT) -DOCRAM_MODE" -p $(ROOT)/os/pal.h -i $< -o $@ || (rm -f $@; exit 1)

