include ../../config.mk

.PHONY: all clean

all: tc ec

clean:
	rm -f tc ec ec.c *.o

OCRAM=../../../../cabal-dev/bin/ocram

tc: tc.o main-tc.o
	$(CPPC) -o $@ $^ -L../../os/ -ltc -los

ec: ec.o main-ec.o
	$(CPPC) -o $@ $^ -L../../os/ -lec -los

ec.c: tc.c $(OCRAM)
	$(OCRAM) -c "-I../.. -DOCRAM_MODE" -i $< -o $@ || (rm $@; exit 1)

%.o: %.c
	$(CC) -Wall -c $< -I../../
