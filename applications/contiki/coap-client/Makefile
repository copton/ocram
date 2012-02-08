DIRS = native generated

.PHONY: all clean test

all:
	for i in $(DIRS); do $(MAKE) -C $$i all || exit 1; done

clean:
	for i in $(DIRS); do $(MAKE) -C $$i clean; done

test:
	for i in $(DIRS); do $(MAKE) -C $$i test; done
