#include <stdio.h>

#include "common.h"
#include "config.h"
#include "contiki.h"
#include "contiki-net.h"
#include "cooja.h"
#include "net/uip.h"
#include "rpc.h"

PROCESS(rpc_client, "RPC Client");
PROCESS_THREAD(rpc_client, ev, data)
{
    static uip_ipaddr_t peer1, peer2;
    static struct etimer et;
    static rpc_call_t call_tell, call_slow_sensor, call_fast_sensor, call_slow_sensor2;
    static rpc_response_t responses[MAX_TELL_DEPTH];
    static struct uip_udp_conn* conn;
    int16_t size;
    bool success;

    PROCESS_BEGIN();

    ipconfig(true);
    uip_ip6addr(&peer1, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202);
    uip_ip6addr(&peer2, 0xfe80, 0, 0, 0, 0x0212, 0x7403, 0x0003, 0x0303);
    conn = udp_new(NULL, 0, NULL);
    udp_bind(conn, UDP_CLIENT_PORT);

    static int counter = 0;
    etimer_set(&et, CALL_INTERVAL);
    for (counter=0; counter<10; counter++) {
        static int pending = 0;
        PROCESS_YIELD_UNTIL(etimer_expired(&et));

        rpc_call_read_slow_sensor(1, &call_slow_sensor);
        rpc_call_tell(&peer2, &call_slow_sensor, &call_tell);
        size = rpc_marshall_call(&call_tell, uip_appdata, UIP_BUFSIZE);
        ASSERT (size != -1);
        printf("trace: "); rpc_print_call(&call_tell);
        uip_udp_packet_sendto(conn, uip_appdata, size, &peer1, UDP_SERVER_PORT);
        pending++;

        rpc_call_read_slow_sensor(2, &call_slow_sensor2);
        size = rpc_marshall_call(&call_slow_sensor2, uip_appdata, UIP_BUFSIZE);
        ASSERT (size != -1);
        printf("trace: "); rpc_print_call(&call_slow_sensor2);
        uip_udp_packet_sendto(conn, uip_appdata, size, &peer1, UDP_SERVER_PORT);
        pending++;

        rpc_call_read_fast_sensor(1, &call_fast_sensor);
        size = rpc_marshall_call(&call_fast_sensor, uip_appdata, UIP_BUFSIZE);
        ASSERT (size != -1);
        printf("trace: "); rpc_print_call(&call_fast_sensor);
        uip_udp_packet_sendto(conn, uip_appdata, size, &peer1, UDP_SERVER_PORT);
        pending++;

        while(pending != 0) {
            PROCESS_YIELD_UNTIL(ev == tcpip_event && uip_newdata());
            success = rpc_unmarshall_response(responses, MAX_TELL_DEPTH, uip_appdata, uip_datalen());
            rpc_response_t* response = responses + 0;
            ASSERT(success == true);
            printf("trace: "); rpc_print_response(response);
            if (0) {
            } else if (response->sequence == call_fast_sensor.sequence) {
                ASSERT(response->function == RPC_READ_FAST_SENSOR);
                pending--;
            } else if (response->sequence == call_tell.sequence) {
                ASSERT(response->function == RPC_TELL);
                ASSERT(response->data.tell.response->sequence == call_tell.data.tell.call->sequence);
                ASSERT(response->data.tell.response->function == RPC_READ_SLOW_SENSOR);
                pending--;
            } else if (response->sequence == call_slow_sensor2.sequence) {
                ASSERT(response->function == RPC_READ_SLOW_SENSOR);
                pending--;
            }
        }

        etimer_reset(&et);
    }

    printf("QUIT\n");

    PROCESS_END();
}

AUTOSTART_PROCESSES(&rpc_client);
