.PHONY: all clean test

CHROOT = schroot -c tinyos -p --

all:
	$(CHROOT) $(MAKE) -f Makefile.tinyos micaz sim

clean:
	$(CHROOT) $(MAKE) -f Makefile.tinyos clean
	git clean -d -x -f

test: all
	$(CHROOT) ./test.py
