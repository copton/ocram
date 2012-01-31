.PHONY: all clean test

all:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki

clean:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki clean

test:
	../test.py `pwd`/cnf-native.csc
