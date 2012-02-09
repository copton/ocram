.PHONY: all clean test

TEST = $(ROOT)/applications/contiki/test.py
VERIFY = $(ROOT)/applications/contiki/collect-and-forward/verify.py

CSC = cnf-native.csc
SKYS = $(shell perl -ne 'if (/<firmware[^>]*>\[CONFIG_DIR\]\/([^<]*)<\/firmware>/) { print "$$1 "; }' < $(CSC))

all:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki

clean:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki clean
	rm -f COOJA.*log OcramCooja.log *.sky symbols.{c,h}

test: all OcramCooja.log

OcramCooja.log: $(TEST) $(VERIFY) $(CSC) $(SKYS)
	$(TEST) $(VERIFY) $(CSC)
