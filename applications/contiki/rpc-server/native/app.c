#include <stdio.h>

#include "contiki.h"
#include "common.h" 
#include "config.h"
#include "rpc.h"

static uint16_t read_slow_sensor(int sensor)
{
    printf("XXX: read_slow_sensor");
    return 23;
}

static uint16_t read_fast_sensor(int sensor)
{
    printf("XXX: read_fast_sensor");
    return 23;
}

static uint16_t tell(uip_ipaddr_t* peer, uint8_t* buffer, size_t len)
{
   printf("XXX: tell"); 
   return 23;
}

static void handle_rpc()
{
    rpc_call_t call;
    rpc_unmarshall_call(&call, uip_appdata, uip_datalen());
    switch (call.function) {
        case RPC_TELL: {
            tell(&call.data.tell.server.peer, call.data.tell.server.buffer, call.data.tell.server.len);
        } break;
        case RPC_READ_SLOW_SENSOR: {
            read_slow_sensor(call.data.read_slow_sensor.sensor);
        } break;
        case RPC_READ_FAST_SENSOR: {
            read_slow_sensor(call.data.read_fast_sensor.sensor);
        } break;
    }
}

PROCESS(server_process, "server process");
PROCESS_THREAD(server_process, ev, data)
{
    static struct uip_udp_conn *server_conn;

    PROCESS_BEGIN();

    ipconfig(false);

    server_conn = udp_new(NULL, UDP_CLIENT_PORT, NULL);
    udp_bind(server_conn, UDP_SERVER_PORT);

    while(1) {
        PROCESS_YIELD_UNTIL(ev == tcpip_event && uip_newdata());
        handle_rpc();
    }

    PROCESS_END();
}

AUTOSTART_PROCESSES(&server_process);

