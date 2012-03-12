DIRS = setup native generated

.PHONY: all clean test compare bench

include $(ROOT)/applications/contiki/config.mak

all:
	for i in $(DIRS); do $(MAKE) -C $$i all || exit 1; done

clean:
	for i in $(DIRS); do $(MAKE) -C $$i clean; done

test:
	for i in $(DIRS); do $(MAKE) -C $$i test || exit 1; done

compare: test $(COMPARE)
	$(COMPARE) `pwd`

bench: compare $(BENCH)
	$(BENCH) `pwd`
