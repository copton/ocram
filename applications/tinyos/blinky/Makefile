.PHONY: all clean test

DIRS=native generated

all:
	for i in $(DIRS); do $(MAKE) -C $$i; done

clean:
	for i in $(DIRS); do $(MAKE) -C $$i; clean

test:
	for i in $(DIRS); do $(MAKE) -C $$i; test
