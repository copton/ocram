.PHONY: all clean test

CHROOT = schroot -c tinyos -p --

all:
	$(CHROOT) $(MAKE) -f Makefile.tinyos micaz sim

clean:
	$(CHROOT) $(MAKE) -f Makefile.tinyos clean

test:
	$(CHROOT) ./test.py
