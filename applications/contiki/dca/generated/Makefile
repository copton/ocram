APP = dca
TYPE = generated
XGCC = msp430-gcc \
	-mmcu=msp430x1611 \
	-DUIP_CONF_IPV6_RPL \
	-DCONTIKI=1 \
	-DCONTIKI_TARGET_SKY=1 \
    -DPROJECT_CONF_H=\"project-conf.h\" \
	-DUIP_CONF_IPV6=1 \
	-DUIP_CONF_IPV6_RPL=1 \
	-DWITH_UIP6=1 \
	-I. \
	-I$(ROOT)/applications/contiki \
	-I/home/alex/scm/contiki/platform/sky \
	-I/home/alex/scm/contiki/platform/sky/dev \
	-I/home/alex/scm/contiki/platform/sky/apps \
	-I/home/alex/scm/contiki/platform/sky/net \
	-I/home/alex/scm/contiki/cpu/msp430/f1xxx \
	-I/home/alex/scm/contiki/cpu/msp430 \
	-I/home/alex/scm/contiki/cpu/msp430/dev \
	-I/home/alex/scm/contiki/core/dev \
	-I/home/alex/scm/contiki/core/lib \
	-I/home/alex/scm/contiki/core/net \
	-I/home/alex/scm/contiki/core/net/mac \
	-I/home/alex/scm/contiki/core/net/rime \
	-I/home/alex/scm/contiki/core/net/rpl \
	-I/home/alex/scm/contiki/core/sys \
	-I/home/alex/scm/contiki/core/cfs \
	-I/home/alex/scm/contiki/core/ctk \
	-I/home/alex/scm/contiki/core/lib/ctk \
	-I/home/alex/scm/contiki/core/loader \
	-I/home/alex/scm/contiki/core \

include $(ROOT)/applications/contiki/common.mak
