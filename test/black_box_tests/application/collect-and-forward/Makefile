include ../../config.mk

all: tc ec

clean:
	rm -f tc ec ec.c *.o

tc: tc.o main-tc.o
	$(CPPC) -o $@ $^ -L../../os/ -ltc -los

ec: ec.o main-ec.o
	$(CPPC) -o $@ $^ -L../../os/ -lec -los

ec.c: tc.c
	../../../../cabal-dev/bin/ocram -c "-I../.. -DOCRAM_MODE" -i $< -o $@ || (rm $@; exit 1)

%.o: %.c
	$(CC) -Wall -c $< -I../../
