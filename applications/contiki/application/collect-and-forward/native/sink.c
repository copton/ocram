#include "contiki.h"
#include "contiki-lib.h"
#include "contiki-net.h"
#include "net/uip.h"
#include "net/rpl/rpl.h"

#include "net/netstack.h"
#include "dev/button-sensor.h"
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

    PRINT6ADDR(&UIP_IP_BUF->srcipaddr);
    printf(": received values: ");
    for (i=0; i<numberof_values; i++) {
        memcpy(&value, &appdata[i * sizeof(uint32_t)], sizeof(uint32_t));
        printf("%lu ", value);
    }
    printf("\n");
}

static void ipconfig(void) {
    struct uip_ds6_addr *root_if;
    uip_ipaddr_t ipaddr;

    // local address
    uip_ip6addr(&ipaddr, 0xaaaa, 0, 0, 0, 0, 0x00ff, 0xfe00, 3);
    uip_ds6_addr_add(&ipaddr, 0, ADDR_MANUAL);

    // RPL stuff...
    root_if = uip_ds6_addr_lookup(&ipaddr);
    if(root_if != NULL) {
        rpl_dag_t *dag;
        dag = rpl_set_root(RPL_DEFAULT_INSTANCE, (uip_ip6addr_t *)&ipaddr);
        uip_ip6addr(&ipaddr, 0xaaaa, 0, 0, 0, 0, 0, 0, 0);
        rpl_set_prefix(dag, &ipaddr, 64);
        PRINTF("created a new RPL dag\n");
    } else {
        PRINTF("failed to create a new RPL DAG\n");
    }

    // udp server connection
    server_conn = udp_new(NULL, UIP_HTONS(UDP_CLIENT_PORT), NULL);
    udp_bind(server_conn, UIP_HTONS(UDP_SERVER_PORT));
    
    // no duty-cycling
    NETSTACK_MAC.off(1);
}

PROCESS_THREAD(udp_server_process, ev, data)
{
    PROCESS_BEGIN();
    PROCESS_PAUSE();

    ipconfig();
    print_local_addresses();

    PRINTF("sink started\n");

    while(1) {
        PROCESS_YIELD();
        if(ev == tcpip_event) {
            tcpip_handler();
        }
    }

    PROCESS_END();
}
