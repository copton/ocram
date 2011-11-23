include ../config.mk

TARGETS = dispatcher.o logger.o random.o ec.o pal.o tc.o libos.a libtc.a libec.a

all: $(TARGETS)

-include depend.mak

clean:
	rm -f $(TARGETS)
	rm -f depend.mak

libos.a: dispatcher.o logger.o random.o ec.o
	$(AR) r $@ $^
	$(RANLIB) $@

libtc.a: tc.o
	$(AR) r $@ $^
	$(RANLIB) $@

libec.a: pal.o
	$(AR) r $@ $^
	$(RANLIB) $@

%.o: %.cc
	$(CPPC) $(CCFLAGS) -c $<

depend.mak:
	$(CPPC) -M *.cc > $@ || rm -f $@
