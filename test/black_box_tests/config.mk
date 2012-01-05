AR=$(TOOLCHAIN)ar
RANLIB=$(TOOLCHAIN)ranlib
CC=$(TOOLCHAIN)gcc
CPPC=$(TOOLCHAIN)g++
OBJDUMP=$(TOOLCHAIN)objdump
STRIP=$(TOOLCHAIN)strip

CCFLAGS=-std=c99 -Wall -I$(ROOT)

ifdef PLATFORM
CCFLAGS+="-DPLATFORM_$(PLATFORM)"
endif

ifdef DEBUG
CCFLAGS+=-g -O0 -Wall
else
CCFLAGS+= -O2
endif

depend.mak:
	$(CC) $(CCFLAGS) -M *.c* > $@ || rm -f $@

%.o: %.cc
	$(CPPC) $(CCFLAGS) -c $<

%.o: %.c
	$(CC) $(CCFLAGS) -c $<
