#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "common.h" 
#include "config.h"
#include "contiki.h"
#include "contiki-net.h"
#include "cooja.h"
#include "net/uip-debug.h"
#include "net/uip.h"
#include "rpc.h"

#include "tl/tl.h"

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
    bool busy;
    condition_t cond;
    rpc_call_t call[MAX_TELL_DEPTH];
    rpc_response_t response[MAX_TELL_DEPTH];
    uip_ipaddr_t peer;
    struct pt pt;
    struct uip_udp_conn* conn;
} worker_t;

condition_t cond_server = TL_CONDITION_INITIALIZER;

#define CALL (&worker->call[0])
#define RESPONSE (&worker->response[0])

void handle_call(worker_t* worker)
{
    printf("trace: "); rpc_print_call(CALL);
    if (0) { 
    } else if(CALL->function == RPC_TELL) {
        int16_t size = rpc_marshall_call(CALL->data.tell.call, uip_appdata, UIP_BUFSIZE);
        ASSERT(size != -1);
        uip_udp_packet_sendto(worker->conn, uip_appdata, size, &CALL->data.tell.peer, UDP_SERVER_PORT);
        do {
            tl_receive(worker->conn, NULL);
        } while (! uip_ipaddr_cmp(&UIP_IP_BUF->srcipaddr, &CALL->data.tell.peer));

        bool success = rpc_unmarshall_response(RESPONSE + 1, MAX_TELL_DEPTH - 1, uip_appdata, uip_datalen());
        ASSERT(success == true);
        rpc_response_tell(RESPONSE + 1, CALL, RESPONSE);
    } else if (CALL->function == RPC_READ_SLOW_SENSOR) {
        tl_sleep(clock_time() + SLOW_SENSOR_DELAY, NULL);
        rpc_response_read_slow_sensor(rand(), CALL, RESPONSE);
    } else if (CALL->function == RPC_READ_FAST_SENSOR) {
        rpc_response_read_fast_sensor(rand(), CALL, RESPONSE);
    } else {
        ASSERT(false);
    }
    
    printf("trace: "); rpc_print_response(RESPONSE);
}

worker_t workers[2];

uint8_t stack_worker0[200];
void worker0()
{
    worker_t* worker = workers + 0;

    worker->conn = udp_new(NULL, 0, NULL);
    ASSERT(worker->conn != NULL);
    udp_bind(worker->conn, UDP_CLIENT_PORT + 0);

    tl_condition_init(&worker->cond);

    while(1) {
        tl_condition_wait(&worker->cond);
        handle_call(worker);
        tl_condition_signal(&cond_server, worker);
    }
}

uint8_t stack_worker1[200];
void worker1()
{
    worker_t* worker = workers + 1;

    worker->conn = udp_new(NULL, 0, NULL);
    ASSERT(worker->conn != NULL);
    udp_bind(worker->conn, UDP_CLIENT_PORT + 1);

    tl_condition_init(&worker->cond);

    while(1) {
        tl_condition_wait(&worker->cond);
        handle_call(worker);
        tl_condition_signal(&cond_server, worker);
    }
}

uint8_t stack_server[200];
void server_thread()
{
    int i=0;
    for (i=0; i<sizeof(workers)/sizeof(worker_t); i++) {
        workers[i].busy = false;
    }

    ipconfig(false);

    struct uip_udp_conn* conn = udp_new(NULL, 0, NULL);
    udp_bind(conn, UDP_SERVER_PORT);

    while(1) {
        if (! tl_receive(conn, &cond_server)) {
            for (i=0; i<sizeof(workers)/sizeof(worker_t); i++) {
                worker_t* worker = workers + i;
                if (worker->busy == false) {
                    worker->busy = true;
                    bool success = rpc_unmarshall_call(CALL, MAX_TELL_DEPTH, uip_appdata, uip_datalen());
                    ASSERT(success == true);
                    worker->peer = UIP_IP_BUF->srcipaddr;
                    tl_condition_signal(&worker->cond, NULL);
                    break;
                }
            }
            ASSERT(i != sizeof(workers)/sizeof(worker_t));
        } else {
            worker_t* worker = cond_server.data;
            worker->busy = false;
            int16_t size = rpc_marshall_response(RESPONSE, uip_appdata, UIP_BUFSIZE);
            ASSERT(size != -1);
            uip_udp_packet_sendto(conn, uip_appdata, size, &worker->peer, UDP_CLIENT_PORT);
        }
    }
}

void tl_app_main()
{
    tl_create_thread(server_thread, stack_server, sizeof(stack_server));
    tl_create_thread(worker0, stack_worker0, sizeof(stack_worker0));
    tl_create_thread(worker1, stack_worker1, sizeof(stack_worker1));
}
