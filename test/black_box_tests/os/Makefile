include ../config.mk
-include depend.mak

.PHONY: all clean

TARGETS = dispatcher.o random.o core.o logger.o pal.o logger_$(PLATFORM).o settings_$(PLATFORM).o tc_$(PLATFORM).o libos.a libtc.a libpal.a

all: $(TARGETS)

clean:
	rm -f *.o *.a depend.mak

libos.a: dispatcher.o random.o core.o logger.o logger_$(PLATFORM).o settings_$(PLATFORM).o
	$(AR) r $@ $^
	$(RANLIB) $@

libtc.a: tc_$(PLATFORM).o
	$(AR) r $@ $^
	$(RANLIB) $@

libpal.a: pal.o
	$(AR) r $@ $^
	$(RANLIB) $@
