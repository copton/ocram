include ../../config.mak

.PHONY: all clean

TARGETS=libos.a libtc.a libpal.a

all: $(TARGETS)

-include depend.mak

clean:
	rm -f $(TARGETS)

libos.a: common.o
	$(AR) r $@ $^
	$(RANLIB) $@

libtc.a: tc.o
	$(AR) r $@ $^
	$(RANLIB) $@

libpal.a: pal.o
	$(AR) r $@ $^
	$(RANLIB) $@
