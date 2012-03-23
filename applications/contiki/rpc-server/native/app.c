#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "common.h" 
#include "config.h"
#include "contiki.h"
#include "contiki-net.h"
#define ASSERT_PRINTF
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

#define UIP_IP_BUF ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])

typedef struct {
    struct process* process;
    bool busy;
    rpc_call_t call[MAX_TELL_DEPTH];
    rpc_response_t response[MAX_TELL_DEPTH];
    uip_ipaddr_t peer;
    struct pt pt;
    struct uip_udp_conn* conn;
} worker_t;

#define CALL (&worker->call[0])
#define RESPONSE (&worker->response[0])

PT_THREAD(worker_thread(worker_t* worker, process_event_t ev)) {
    PT_BEGIN(&worker->pt);

    rpc_print_call(CALL);
    if (0) { 
    } else if(CALL->function == RPC_TELL) {
        printf("trace: telling peer "); uip_debug_ipaddr_print(&CALL->data.tell.peer); printf("\n");
        int16_t size = rpc_marshall_call(CALL->data.tell.call, uip_appdata, UIP_BUFSIZE);
        ASSERT(size != -1);
        uip_udp_packet_sendto(worker->conn, uip_appdata, size, &CALL->data.tell.peer, UDP_SERVER_PORT);
        PT_YIELD_UNTIL(&worker->pt, ev == tcpip_event && uip_newdata() && uip_ipaddr_cmp(&UIP_IP_BUF->srcipaddr, &CALL->data.tell.peer));

        bool success = rpc_unmarshall_response(RESPONSE + 1, MAX_TELL_DEPTH - 1, uip_appdata, uip_datalen());
        ASSERT(success == true);
        rpc_response_tell(RESPONSE + 1, CALL, RESPONSE);
        printf("trace: sending tell response\n");
    } else if (CALL->function == RPC_READ_SLOW_SENSOR) {
        printf("trace: reading slow sensor: %d\n", CALL->data.read_slow_sensor.sensor);
        static struct etimer et;
        etimer_set(&et, SLOW_SENSOR_DELAY);
        PT_YIELD_UNTIL(&worker->pt, etimer_expired(&et));
        rpc_response_read_slow_sensor(rand(), CALL, RESPONSE);
        printf("trace: sending slow sensor value: %d\n", RESPONSE->data.read_slow_sensor.value);
    } else if (CALL->function == RPC_READ_FAST_SENSOR) {
        printf("trace: reading fast sensor: %d\n", CALL->data.read_fast_sensor.sensor);
        rpc_response_read_fast_sensor(rand(), CALL, RESPONSE);
        printf("trace: sending fast sensor value: %d\n", RESPONSE->data.read_fast_sensor.value);
    } else {
        ASSERT(false);
    }
    
    rpc_print_response(RESPONSE);
    PT_END(&worker->pt);
}

PROCESS(server_process, "server process");
worker_t workers[2];
PROCESS(worker0, "worker0");
PROCESS(worker1, "worker1");
AUTOSTART_PROCESSES(&server_process, &worker0, &worker1);

PROCESS_THREAD(worker0, ev, data)
{
    worker_t* worker = workers + 0;

    PROCESS_BEGIN();

    worker->conn = udp_new(NULL, 0, NULL);
    ASSERT(worker->conn != NULL);
    udp_bind(worker->conn, UDP_CLIENT_PORT + 0);

    while(1) {
        PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);
        PT_SPAWN(process_pt, &worker->pt, worker_thread(worker, ev));
        process_post(&server_process, PROCESS_EVENT_CONTINUE, worker);
    }
    PROCESS_END();
}

PROCESS_THREAD(worker1, ev, data)
{
    worker_t* worker = workers + 1;

    PROCESS_BEGIN();

    worker->conn = udp_new(NULL, 0, NULL);
    ASSERT(worker->conn != NULL);
    udp_bind(worker->conn, UDP_CLIENT_PORT + 1);

    while(1) {
        PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);
        PT_SPAWN(process_pt, &worker->pt, worker_thread(worker, ev));
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

    conn = udp_new(NULL, 0, NULL);
    udp_bind(conn, UDP_SERVER_PORT);

    while(1) {
        PROCESS_YIELD();
        if (ev == tcpip_event && uip_newdata()) {
            printf("XXX: received packet from "); uip_debug_ipaddr_print(&UIP_IP_BUF->srcipaddr); printf(": "); print_buffer(uip_appdata, uip_datalen()); printf("\n");

            for (i=0; i<sizeof(workers)/sizeof(worker_t); i++) {
                worker_t* worker = workers + i;
                if (worker->busy == false) {
                    worker->busy = true;
                    bool success = rpc_unmarshall_call(&worker->call[0], MAX_TELL_DEPTH, uip_appdata, uip_datalen());
                    rpc_print_call(&worker->call[0]);
                    ASSERT(success == true);
                    worker->peer = UIP_IP_BUF->srcipaddr;
                    process_poll(worker->process);
                    break;
                }
            }
            ASSERT(i != sizeof(workers)/sizeof(worker_t));
        } else if (ev == PROCESS_EVENT_CONTINUE) {
            worker_t* worker = data;
            worker->busy = false;
            int16_t size = rpc_marshall_response(&worker->response[0], uip_appdata, UIP_BUFSIZE);
            ASSERT(size != -1);
            printf("XXX: sending packet to "); uip_debug_ipaddr_print(&worker->peer); printf(": "); print_buffer(uip_appdata, size); printf("\n");
            uip_udp_packet_sendto(conn, uip_appdata, size, &worker->peer, UDP_CLIENT_PORT);
        }
    }

    PROCESS_END();
}

