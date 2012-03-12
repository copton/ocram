DIRS=$(shell find . -mindepth 1 -maxdepth 1 -type d ! -name os)

.PHONY: all clean test compare bench

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
