.PHONY: all clean

NOOB_OPTIONS=-deprecation -unchecked -explaintypes
SC=fsc

all:
	$(SC) $(NOOB_OPTIONS) -classpath libs/xtc.jar -d target src/**/*scala

clean:
	rm -fr target/*
