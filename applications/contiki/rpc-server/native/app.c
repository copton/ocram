#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "common.h" 
#include "config.h"
#include "contiki.h"
#include "contiki-net.h"
#include "debug.h"
#include "net/uip-debug.h"
#include "net/uip.h"
#include "rpc.h"

// rand is not standard compliant on this platform and sometimes returns negative values
int rand_fixed()
{
    int value;
    do {
        value = rand();
    } while (value < 0);
    return value;
}

PROCESS(server_process, "server process");
PROCESS(worker0, "worker0");
PROCESS(worker1, "worker1");

#define UIP_IP_BUF ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])

typedef struct {
    struct process* process;
    bool busy;
    rpc_call_t call;
    rpc_response_t response;
    uip_ipaddr_t peer;
} worker_t;

worker_t workers[2];

PROCESS_THREAD(worker0, ev, data)
{
    worker_t* worker = workers + 0;
    static struct uip_udp_conn* conn;
    PROCESS_BEGIN();

    conn = udp_new(NULL, UDP_SERVER_PORT, NULL);
    udp_bind(conn, UDP_CLIENT_PORT);

    while(1) {
        PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);
        if (0) { 
        } else if(worker->call.function == RPC_TELL) {
            printf("trace: telling peer "); uip_debug_ipaddr_print(&worker->call.data.tell.server.peer); printf("\n");
            uip_udp_packet_sendto(conn, worker->call.data.tell.server.buffer, worker->call.data.tell.server.len, &worker->call.data.tell.server.peer, UDP_SERVER_PORT);
            PROCESS_YIELD_UNTIL(ev == tcpip_event && uip_newdata() && uip_ipaddr_cmp(&UIP_IP_BUF->srcipaddr, &worker->call.data.tell.server.peer));
            rpc_response_tell(&worker->call, &worker->response);
            printf("trace: got reply from peer\n");
        } else if (worker->call.function == RPC_READ_SLOW_SENSOR) {
            printf("trace: reading slow sensor: %d\n", worker->call.data.read_slow_sensor.sensor);
            static struct etimer et;
            etimer_set(&et, SLOW_SENSOR_DELAY);
            PROCESS_YIELD_UNTIL(etimer_expired(&et));
            rpc_response_read_slow_sensor(rand(), &worker->call, &worker->response);
            printf("trace: sending slow sensor value: %d\n", worker->response.data.read_slow_sensor.value);
        } else if (worker->call.function == RPC_READ_FAST_SENSOR) {
            printf("trace: reading fast sensor: %d\n", worker->call.data.read_fast_sensor.sensor);
            rpc_response_read_fast_sensor(rand(), &worker->call, &worker->response);
            printf("trace: sending fast sensor value: %d\n", worker->response.data.read_fast_sensor.value);
        }
        process_post(&server_process, PROCESS_EVENT_CONTINUE, worker);
    }
    PROCESS_END();
}

PROCESS_THREAD(worker1, ev, data)
{
    worker_t* worker = workers + 1;
    static struct uip_udp_conn* conn;
    PROCESS_BEGIN();

    conn = udp_new(NULL, UDP_SERVER_PORT, NULL);
    udp_bind(conn, UDP_CLIENT_PORT);

    while(1) {
        PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);
        if (0) { 
        } else if(worker->call.function == RPC_TELL) {
            printf("trace: telling peer "); uip_debug_ipaddr_print(&worker->call.data.tell.server.peer); printf("\n");
            uip_udp_packet_sendto(conn, worker->call.data.tell.server.buffer, worker->call.data.tell.server.len, &worker->call.data.tell.server.peer, UDP_SERVER_PORT);
            PROCESS_YIELD_UNTIL(ev == tcpip_event && uip_newdata() && uip_ipaddr_cmp(&UIP_IP_BUF->srcipaddr, &worker->call.data.tell.server.peer));
            rpc_response_tell(&worker->call, &worker->response);
            printf("trace: got reply from peer\n");
        } else if (worker->call.function == RPC_READ_SLOW_SENSOR) {
            printf("trace: reading slow sensor: %d\n", worker->call.data.read_slow_sensor.sensor);
            static struct etimer et;
            etimer_set(&et, SLOW_SENSOR_DELAY);
            PROCESS_YIELD_UNTIL(etimer_expired(&et));
            rpc_response_read_slow_sensor(rand(), &worker->call, &worker->response);
            printf("trace: sending slow sensor value: %d\n", worker->response.data.read_slow_sensor.value);
        } else if (worker->call.function == RPC_READ_FAST_SENSOR) {
            printf("trace: reading fast sensor: %d\n", worker->call.data.read_fast_sensor.sensor);
            rpc_response_read_fast_sensor(rand(), &worker->call, &worker->response);
            printf("trace: sending fast sensor value: %d\n", worker->response.data.read_fast_sensor.value);
        }
        process_post(&server_process, PROCESS_EVENT_CONTINUE, worker);
    }
    PROCESS_END();
}

PROCESS_THREAD(server_process, ev, data)
{
    static struct uip_udp_conn *conn;

    PROCESS_BEGIN();

    int i = 0;
    for (i=0; i<sizeof(workers)/sizeof(worker_t); i++) {
        workers[i].busy = false;
    }
    workers[0].process = &worker0;
    workers[1].process = &worker1;

    ipconfig(false);

    conn = udp_new(NULL, UDP_CLIENT_PORT, NULL);
    udp_bind(conn, UDP_SERVER_PORT);

    while(1) {
        PROCESS_YIELD();
        if (ev == tcpip_event && uip_newdata()) {
            printf("XXX: received %d bytes from ", uip_datalen()); uip_debug_ipaddr_print(&workers[i].peer); printf("\n");
            for (i=0; i<sizeof(workers)/sizeof(worker_t); i++) {
                worker_t* worker = workers + i;
                if (worker->busy == false) {
                    worker->busy = true;
                    rpc_unmarshall_call(&worker->call, uip_appdata, uip_datalen());
                    worker->peer = UIP_IP_BUF->srcipaddr;
                    process_poll(workers[i].process);
                    break;
                }
            }
            ASSERT(i != sizeof(workers)/sizeof(worker_t));
        } else if (ev == PROCESS_EVENT_CONTINUE) {
            worker_t* worker = data;
            worker->busy = false;
            int16_t size = rpc_marshall_response(&worker->response, uip_appdata, UIP_BUFSIZE);
            ASSERT(size != -1);
            printf("XXX: sending %d bytes to ", size); uip_debug_ipaddr_print(&worker->peer); printf("\n");
            uip_udp_packet_sendto(conn, uip_appdata, size, &worker->peer, UDP_CLIENT_PORT);
        }
    }

    PROCESS_END();
}

AUTOSTART_PROCESSES(&server_process, &worker0, &worker1);

