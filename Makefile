.PHONY: all clean

NOOB_OPTIONS=-deprecation -unchecked -explaintypes
SC=fsc

all:
	$(SC) $(NOOB_OPTIONS) -classpath libs/xtc.jar -d target `find src -type f -name "*.scala"`

clean:
	rm -fr target/*
