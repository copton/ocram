.PHONY: all contiki clean test

include $(ROOT)/applications/contiki/config.mak

all: contiki server.cached.sky

server.cached.sky: $(CACHE) server.sky
	$^

test: all

include $(ROOT)/applications/contiki/goals.mak
