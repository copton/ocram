.PHONY: all contiki clean test

include $(ROOT)/applications/contiki/config.mak

SKYS=source.cached.sky sink.cached.sky

all: contiki $(SKYS)

$(SKYS): %.cached.sky: $(CACHE) %.sky
	$^

test: all

include $(ROOT)/applications/contiki/goals.mak
