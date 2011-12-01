.PHONY: all clean

all:
	$(MAKE) -C $(PLATFORM) all
	rm -f platform
	ln -s $(PLATFORM) platform

clean:
	$(MAKE) -C $(PLATFORM) clean
	rm -f platform
