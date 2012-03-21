#include <stdio.h>

#include "common.h"
#include "config.h"
#include "contiki.h"
#include "contiki-net.h"
#include "debug.h"
#include "net/uip.h"
#include "rpc.h"

PROCESS(rpc_client, "RPC Client");
PROCESS_THREAD(rpc_client, ev, data)
{
    static uip_ipaddr_t peer;
    static struct etimer et;
    static rpc_call_t call_tell, call_remote, call_fast_sensor, call_slow_sensor;
    static rpc_response_t response;
    static struct uip_udp_conn* conn;
    int16_t size;
    bool success;

    PROCESS_BEGIN();

    ipconfig(true);
    uip_ip6addr(&peer, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202);
    conn = udp_new(NULL, 0, NULL);
    udp_bind(conn, UDP_CLIENT_PORT);

    etimer_set(&et, CALL_INTERVAL);
    while(1) {
        static int pending = 0;
        PROCESS_YIELD_UNTIL(etimer_expired(&et));

        rpc_call_read_slow_sensor(1, &call_slow_sensor);
        size = rpc_marshall_call(&call_slow_sensor, uip_appdata, UIP_BUFSIZE);
        ASSERT(size != -1);
        printf("trace: calling slow sensor 1\n");
        uip_udp_packet_sendto(conn, uip_appdata, size, &peer, UDP_SERVER_PORT);
        pending++;

        rpc_call_read_fast_sensor(1, &call_fast_sensor);
        size = rpc_marshall_call(&call_fast_sensor, uip_appdata, UIP_BUFSIZE);
        ASSERT (size != -1);
        printf("trace: calling fast sensor 1\n");
        uip_udp_packet_sendto(conn, uip_appdata, size, &peer, UDP_SERVER_PORT);
        pending++;

        while(pending != 0) {
            PROCESS_YIELD_UNTIL(ev == tcpip_event && uip_newdata());
            success = rpc_unmarshall_response(&response, uip_appdata, uip_datalen());
            ASSERT(success == true);
            if (0) {
            } else if (response.sequence == call_fast_sensor.sequence) {
                ASSERT(response.function == RPC_READ_FAST_SENSOR);
                printf("trace: received value from fast sensor: %d\n", response.data.read_fast_sensor.value);
                pending--;
            } else if (response.sequence == call_slow_sensor.sequence) {
                ASSERT(response.function == RPC_READ_SLOW_SENSOR);
                printf("trace: received value from slow sensor: %d\n", response.data.read_fast_sensor.value);
                pending--;
            }
        }

        etimer_reset(&et);
    }

    PROCESS_END();
}

AUTOSTART_PROCESSES(&rpc_client);
