include ../config.mk

TARGETS = dispatcher.o logger.o random.o ec.o pal.o tc.o libos.a

all: $(TARGETS)

clean:
	rm -f $(TARGETS)

libos.a: dispatcher.o logger.o random.o ec.o
	$(AR) r $@ $^
	$(RANLIB) $@

%.o: %.cc
	$(CPPC) -c $<

%.o: %.c
	$(CC) -c $<
