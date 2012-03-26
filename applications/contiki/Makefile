DIRS= dca coap rpc

.PHONY: all clean test compare bench plot

all:
	for i in $(DIRS); do $(MAKE) -C $$i all || exit 1; done

clean:
	for i in $(DIRS); do $(MAKE) -C $$i clean; done

test:
	for i in $(DIRS); do $(MAKE) -C $$i test || exit 1; done

compare:
	for i in $(DIRS); do $(MAKE) -C $$i compare || exit 1; done

bench:
	for i in $(DIRS); do $(MAKE) -C $$i bench || exit 1; done

plot: $(ROOT)/applications/contiki/plot.py bench
	$<
