AR=$(TOOLCHAIN)ar
RANLIB=$(TOOLCHAIN)ranlib
CC=$(TOOLCHAIN)gcc
CPPC=$(TOOLCHAIN)g++
OBJDUMP=$(TOOLCHAIN)objdump
STRIP=$(TOOLCHAIN)strip

CFLAGS+=-std=c99 -Wall -I$(ROOT)

ifdef DEBUG
CFLAGS+=-g -O0 -Wall
else
CFLAGS+= -O2
endif

depend.mak:
	$(CC) $(CFLAGS) -M *.c* > $@ || rm -f $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<
