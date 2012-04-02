#include "contiki.h"
#include "contiki-lib.h"
#include "contiki-net.h"
#include "dev/button-sensor.h"
#include "net/netstack.h"
#include "net/uip.h"
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


#define DEBUG DEBUG_PRINT
#include "net/uip-debug.h"

#include "common.h"

#define UIP_IP_BUF   ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])

static struct uip_udp_conn *server_conn;

PROCESS(udp_server_process, "UDP server process");
AUTOSTART_PROCESSES(&udp_server_process);

static void tcpip_handler(void) {
    static int counter = 0;

    char* appdata = (char*) uip_appdata;
    size_t len = uip_datalen();
    size_t numberof_values = len / sizeof(uint32_t);
    uint32_t value;
    size_t i;

    if(! uip_newdata()) {
        printf("tcpip handler called for no reason(?)\n");
        return;
    }

    if ((len % sizeof(uint32_t)) != 0) {
        printf("ignoring packet of unexpected size\n");
        return;
    }

    counter++;

    printf("trace: received values: %d/5: ", counter);
    for (i=0; i<numberof_values; i++) {
        memcpy(&value, &appdata[i * sizeof(uint32_t)], sizeof(uint32_t));
        printf("%lu ", value);
    }
    printf("\n");

    if (counter == 5) {
        printf("QUIT\n");
    }
}

static void init() {
    ipconfig(true);

    server_conn = udp_new(NULL, UIP_HTONS(UDP_CLIENT_PORT), NULL);
    udp_bind(server_conn, UIP_HTONS(UDP_SERVER_PORT));
}

PROCESS_THREAD(udp_server_process, ev, data)
{
    PROCESS_BEGIN();
    PROCESS_PAUSE();

    init();

    PRINTF("sink started\n");

    while(1) {
        PROCESS_YIELD();
        if(ev == tcpip_event) {
            tcpip_handler();
        }
    }

    PROCESS_END();
}
