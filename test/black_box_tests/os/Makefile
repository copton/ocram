include ../config.mk

TARGETS = dispatcher.o logger.o random.o file.o core.o pal.o tc.o libos.a libtc.a libpal.a

all: $(TARGETS)

-include depend.mak

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

%.o: %.cc
	$(CPPC) $(CCFLAGS) -c $<

depend.mak:
	$(CPPC) -M *.cc > $@ || rm -f $@
