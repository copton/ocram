.PHONY: all contiki clean test

include $(ROOT)/applications/contiki/config.mak

SKYS=client.cached.sky peer.cached.sky

all: contiki $(SKYS)

test: all

include $(ROOT)/applications/contiki/goals.mak
