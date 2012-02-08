.PHONY: all clean test

TEST = $(ROOT)/applications/contiki/test.py
VERIFY = $(ROOT)/applications/contiki/coap-client/verify.py

all:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki

clean:
	schroot -c contiki -- $(MAKE) -f Makefile.contiki clean
	rm -f test.log

test: test.log
   
test.log: all coap-client-native.csc $(TEST) $(VERIFY)
	$(TEST) $(VERIFY) coap-client-native.csc
