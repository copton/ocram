DIRS=$(shell find . -mindepth 1 -maxdepth 1 -type d ! -name os)

.PHONY: all clean test bench

all:
	for i in $(DIRS); do $(MAKE) -C $$i all || exit 1; done

clean:
	for i in $(DIRS); do $(MAKE) -C $$i clean; done

test:
	for i in $(DIRS); do $(MAKE) -C $$i test; done

bench:
	for i in $(DIRS); do $(MAKE) -C $$i bench; done

