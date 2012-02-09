DIRS = os $(shell find . -mindepth 1 -maxdepth 1 -type d ! -name os)

.PHONY: all clean test bench

all: platform
	for i in $(DIRS); do $(MAKE) -C $$i all || exit 1; done

clean:
	for i in $(DIRS); do $(MAKE) -C $$i clean; done

test: platform
	for i in $(DIRS); do $(MAKE) -C $$i test; done

bench: platform
	for i in $(DIRS); do $(MAKE) -C $$i bench; done

platform:
	if [ _$$PLATFORM = _ ]; then echo "PLATFORM is not defined" >&2; exit 1; fi
	rm -f os/platform
	ln -s $(PLATFORM) os/platform
