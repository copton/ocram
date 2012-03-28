DIRS= dca coap rpc1 rpc2 rpc3

.PHONY: all clean test compare bench plot

PLOT = $(ROOT)/applications/contiki/plot.py

all:
	for i in $(DIRS); do $(MAKE) -C $$i all || exit 1; done

clean:
	git clean -d -x -f

test:
	for i in $(DIRS); do $(MAKE) -C $$i test || exit 1; done

compare:
	for i in $(DIRS); do $(MAKE) -C $$i compare || exit 1; done

bench:
	for i in $(DIRS); do $(MAKE) -C $$i bench || exit 1; done

plot: $(PLOT) bench
	$<
