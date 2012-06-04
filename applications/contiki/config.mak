ifndef ROOT
$(error no ROOT defined)
endif

CHROOT = schroot -c contiki -p --
TEST = $(ROOT)/applications/contiki/test.py
COMPARE= $(ROOT)/applications/contiki/compare.py
BENCH = $(ROOT)/applications/contiki/bench.py
