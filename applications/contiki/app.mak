DIRS = setup native generated runtime

.PHONY: all clean test compare bench

include $(ROOT)/applications/contiki/config.mak

all:
	for i in $(DIRS); do $(MAKE) -C $$i all || exit 1; done

clean:
	git clean -d -x -f

test:
	for i in $(DIRS); do $(MAKE) -C $$i test || exit 1; done

compare: test $(COMPARE)
	$(COMPARE) trace `pwd`
	$(COMPARE) observe `pwd`

bench: compare $(BENCH)
	$(BENCH) `pwd`
