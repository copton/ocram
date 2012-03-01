.PHONY: all contiki clean test

all: contiki server.cached.sky

include $(ROOT)/applications/contiki/config.mak

server.cached.sky: $(CACHE) server.sky
	$^

test: all
