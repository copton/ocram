include ../config.mk
-include depend.mak

.PHONY: all clean

TARGETS = dispatcher.o random.o core.o logger.o logger_$(PLATFORM).o settings_$(PLATFORM).o tc_$(PLATFORM).o

all: $(TARGETS)

clean:
	rm -f *.o depend.mak
