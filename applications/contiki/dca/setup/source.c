#include "contiki.h"
#include "lib/random.h"
#include "sys/ctimer.h"
#include "net/uip.h"
#include "net/uip-ds6.h"
#include "net/uip-udp-packet.h"
#include "sys/ctimer.h"
#include "net/netstack.h"
#include "config.h"

#include <stdio.h>
#include <string.h>

#define DEBUG DEBUG_PRINT
#include "net/uip-debug.h"

#include "common.h"

static struct uip_udp_conn *client_conn;
static uip_ipaddr_t server_ipaddr;


PROCESS(udp_client_process, "UDP client process");
AUTOSTART_PROCESSES(&udp_client_process);

static void send_packet() {
    uint32_t values[2] = {random_rand(), random_rand()};

    printf("trace: send values: %lu %lu\n", values[0], values[1]);
    uip_udp_packet_sendto(client_conn, values, sizeof(values), &server_ipaddr, UIP_HTONS(UDP_SERVER_PORT));
}

static void init() {
    ipconfig(false);
    uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x212, 0x7402, 0x2, 0x202);
    client_conn = udp_new(NULL, UIP_HTONS(UDP_SERVER_PORT), NULL); 
    udp_bind(client_conn, UIP_HTONS(UDP_CLIENT_PORT)); 
}

PROCESS_THREAD(udp_client_process, ev, data)
{
    static struct etimer periodic;

    PROCESS_BEGIN();
    PROCESS_PAUSE();

    init();

    PRINTF("source started\n");

    etimer_set(&periodic, DT_SOURCE);
    while(1) {
        PROCESS_YIELD();
        if(ev == tcpip_event) {
            printf("ignoring received TCP packet\n");
        }

        if(etimer_expired(&periodic)) {
            etimer_reset(&periodic);
            send_packet();
        }
    }

    PROCESS_END();
}
