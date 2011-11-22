include ../config.mk

TARGETS = dispatcher.o logger.o random.o ec.o pal.o tc.o libos.a

all: $(TARGETS)

-include depend.mak

clean:
	rm -f $(TARGETS)
	rm -f depend.mak

libos.a: dispatcher.o logger.o random.o ec.o
	$(AR) r $@ $^
	$(RANLIB) $@

%.o: %.cc
	$(CPPC) -c $<

%.o: %.c
	$(CC) -c $<

depend.mak:
	$(CPPC) -M *.cc > $@ || rm -f $@
