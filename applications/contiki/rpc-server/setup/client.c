#include <stdio.h>

#include "contiki.h"
#include "contiki-net.h"
#include "net/uip.h"
#include "debug.h"
#include "rpc.h"
#include "config.h"

PROCESS(rpc_client, "RPC Client");
PROCESS_THREAD(rpc_client, ev, data)
{
    static uip_ipaddr_t peer;
    static struct etimer et;
    static rpc_call_t call;
    static rpc_response_t response;
    static uint8_t buffer[100];
    static struct uip_udp_conn* conn;
    int16_t size;
    bool success;

    PROCESS_BEGIN();

    uip_ip6addr(&peer, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202);
    conn = udp_new(NULL, UIP_HTONS(UDP_SERVER_PORT), NULL);
    udp_bind(conn, UIP_HTONS(UDP_CLIENT_PORT));

    etimer_set(&et, CALL_INTERVAL);
    while(1) {
        PROCESS_YIELD_UNTIL(etimer_expired(&et));
        rpc_call_read_fast_sensor(1, &call);
        size = rpc_marshall_call(&call, buffer, sizeof(buffer));
        ASSERT (size != -1);
        printf("trace: calling fast sensor 1\n");
        uip_udp_packet_sendto(conn, buffer, size, &peer, UDP_SERVER_PORT);

        PROCESS_YIELD_UNTIL(ev == tcpip_event && uip_newdata());
        success = rpc_unmarshall_response(&response, uip_appdata, uip_datalen());
        ASSERT(success == true);
        ASSERT(response.sequence == call.sequence);
        ASSERT(response.function == call.function);
        printf("trace: receiving value: %d\n", response.data.read_fast_sensor.value);

        etimer_reset(&et);
    }

    PROCESS_END();
}

AUTOSTART_PROCESSES(&rpc_client);
