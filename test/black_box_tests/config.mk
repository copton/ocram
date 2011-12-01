AR=$(TOOLCHAIN)ar
RANLIB=$(TOOLCHAIN)ranlib
CC=$(TOOLCHAIN)gcc
CPPC=$(TOOLCHAIN)g++
OBJDUMP=$(TOOLCHAIN)objdump
STRIP=$(TOOLCHAIN)strip

CCFLAGS=-Wall -I$(ROOT)

ifdef PLATFORM
CCFLAGS+="-DPLATFORM_$(PLATFORM)"
endif

ifdef DEBUG
CCFLAGS+=-g -O0 -Wall
else
CCFLAGS+= -O2
endif

depend.mak:
	$(CPPC) $(CCFLAGS) -M *.cc > $@ || rm -f $@

%.o: %.cc
	$(CPPC) $(CCFLAGS) -c $<
