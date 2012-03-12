# assume that config.mak has already been included
contiki:
	$(CHROOT) $(MAKE) -f Makefile.contiki

clean:
	$(CHROOT) $(MAKE) -f Makefile.contiki clean
	git clean -d -x -f
