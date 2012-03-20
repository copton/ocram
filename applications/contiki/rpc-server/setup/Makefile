.PHONY: all contiki clean test

include $(ROOT)/applications/contiki/config.mak

all: contiki client.cached.sky

client.cached.sky: $(CACHE) client.sky
	$^

test: all

include $(ROOT)/applications/contiki/goals.mak
