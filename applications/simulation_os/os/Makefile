include ../config.mk
-include depend.mak

.PHONY: all clean test bench

TARGETS = dispatcher.o random.o core.o logger.o settings.o logger_platform.o tc.o libos.a libtc.a

all: $(TARGETS)

clean:
	git clean -d -f -x

test: all

bench: test


libos.a: dispatcher.o random.o core.o logger.o logger_platform.o settings.o
	$(AR) r $@ $^
	$(RANLIB) $@

libtc.a: tc.o
	$(AR) r $@ $^
	$(RANLIB) $@
