ifndef ROOT
$(error no ROOT defined)
endif

TEST = $(ROOT)/applications/contiki/test.py
CACHE = $(ROOT)/applications/contiki/cache.py
CHROOT = schroot -c contiki -p --
