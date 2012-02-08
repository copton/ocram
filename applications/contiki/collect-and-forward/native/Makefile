.PHONY: all clean test

all:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki

clean:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki clean
	rm -f test.log

test: test.log

test.log: all cnf-native.csc ../test.py
	../test.py `pwd`/cnf-native.csc
	cp /tmp/OcramCooja.log $@
