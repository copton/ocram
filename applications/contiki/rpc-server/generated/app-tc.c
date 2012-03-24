#include <stdio.h>
#include <stdlib.h>
#include "tc/tc_receive.h"
#include "tc/tc_send.h"
#include "tc/tc_sleep.h"
#include "tc/tc_condition.h"

#include "rpc.h"
#include "common.h"
#include "config.h"
#include "debug.h"

#include "net/uip.h"
#include "net/uip-udp-packet.h"

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

condition_t cond_server = TC_CONDITION_INITIALIZER;

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
            tc_receive(NULL);
        } while (! uip_ipaddr_cmp(&UIP_IP_BUF->srcipaddr, &CALL->data.tell.peer));

        bool success = rpc_unmarshall_response(RESPONSE + 1, MAX_TELL_DEPTH - 1, uip_appdata, uip_datalen());
        ASSERT(success == true);
        rpc_response_tell(RESPONSE + 1, CALL, RESPONSE);
    } else if (CALL->function == RPC_READ_SLOW_SENSOR) {
        tc_sleep(clock_time() + SLOW_SENSOR_DELAY, NULL);
        rpc_response_read_slow_sensor(rand(), CALL, RESPONSE);
    } else if (CALL->function == RPC_READ_FAST_SENSOR) {
        rpc_response_read_fast_sensor(rand(), CALL, RESPONSE);
    } else {
        ASSERT(false);
    }
    
    printf("trace: "); rpc_print_response(RESPONSE);
}

worker_t workers[2];

TC_RUN_THREAD void worker0()
{
    worker_t* worker = workers + 0;

    worker->conn = udp_new(NULL, 0, NULL);
    ASSERT(worker->conn != NULL);
    udp_bind(worker->conn, UDP_CLIENT_PORT + 0);

    tc_condition_init(&worker->cond);

    while(1) {
        tc_condition_wait(&worker->cond);
        handle_call(worker);
        tc_condition_signal(&cond_server, worker);
    }
}

TC_RUN_THREAD void worker1()
{
    worker_t* worker = workers + 1;

    worker->conn = udp_new(NULL, 0, NULL);
    ASSERT(worker->conn != NULL);
    udp_bind(worker->conn, UDP_CLIENT_PORT + 1);

    tc_condition_init(&worker->cond);

    while(1) {
        tc_condition_wait(&worker->cond);
        handle_call(worker);
        tc_condition_signal(&cond_server, worker);
    }
}

TC_RUN_THREAD void server_thread()
{
    int i=0;
    for (i=0; i<sizeof(workers)/sizeof(worker_t); i++) {
        workers[i].busy = false;
    }

    ipconfig(false);

    struct uip_udp_conn* conn = udp_new(NULL, 0, NULL);
    udp_bind(conn, UDP_SERVER_PORT);

    while(1) {
        if (! tc_receive(&cond_server)) {
            for (i=0; i<sizeof(workers)/sizeof(worker_t); i++) {
                worker_t* worker = workers + i;
                if (worker->busy == false) {
                    worker->busy = true;
                    bool success = rpc_unmarshall_call(CALL, MAX_TELL_DEPTH, uip_appdata, uip_datalen());
                    ASSERT(success == true);
                    worker->peer = UIP_IP_BUF->srcipaddr;
                    tc_condition_signal(&worker->cond, NULL);
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
