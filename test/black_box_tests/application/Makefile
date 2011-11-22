DIRS=$(shell find . -mindepth 1 -maxdepth 1 -type d)

.PHONY: all clean

all:
	for i in $(DIRS); do $(MAKE) -C $$i all; done

clean:
	for i in $(DIRS); do $(MAKE) -C $$i clean; done
