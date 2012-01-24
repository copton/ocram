include ../config.mk
-include depend.mak

.PHONY: all clean

TARGETS = dispatcher.o random.o core.o logger.o settings.o logger_platform.o tc.o libos.a libtc.a

all: $(TARGETS)

clean:
	rm -f *.o *.a depend.mak

libos.a: dispatcher.o random.o core.o logger.o logger_platform.o settings.o
	$(AR) r $@ $^
	$(RANLIB) $@

libtc.a: tc.o
	$(AR) r $@ $^
	$(RANLIB) $@
