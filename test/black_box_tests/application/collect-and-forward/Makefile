include ../../config.mk

.PHONY: all clean

all: tc.elf ec.elf

clean:
	rm -f tc.elf ec.elf ec.c *.o

OCRAM=../../../../cabal-dev/bin/ocram

tc.elf: tc.o main-tc.o ../../os/libtc.a ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^ -lpthread

ec.elf: ec.o ../../os/libpal.a ../../os/libos.a
	$(CPPC) $(CCFLAGS) -o $@ $^

ec.c: tc.c $(OCRAM)
	$(OCRAM) -c "-I../.. -DOCRAM_MODE" -i $< -o $@ || (rm $@; exit 1)

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I../../
