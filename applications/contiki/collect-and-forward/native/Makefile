.PHONY: all clean test

TEST = $(ROOT)/applications/contiki/test.py
VERIFY = $(ROOT)/applications/contiki/collect-and-forward/verify.py

all:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki

clean:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki clean
	rm -f test.log COOJA.log COOKA.testlog

test: all test.log

test.log: $(TEST) $(VERIFY) cnf-native.csc
	$^
