include ../../config.mk
-include depend.mak

.PHONY: all clean

TARGETS = dispatcher.o logger.o random.o file.o core.o pal.o tc.o libos.a libtc.a libpal.a

all: $(TARGETS)

clean:
	rm -f $(TARGETS)
	rm -f depend.mak

libos.a: dispatcher.o logger.o random.o file.o core.o
	$(AR) r $@ $^
	$(RANLIB) $@

libtc.a: tc.o
	$(AR) r $@ $^
	$(RANLIB) $@

libpal.a: pal.o
	$(AR) r $@ $^
	$(RANLIB) $@

