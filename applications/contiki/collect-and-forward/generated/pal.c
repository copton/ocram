#include "contiki.h"
#include "clock.h"
#include "contiki-net.h"
#include "net/netstack.h"
#include "net/uip.h"
#include "net/uiplib.h"
#include "types.h"

#include "pal.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

void ec_thread_1(void*);
void ec_thread_2(void*);
void ec_thread_3(void*);

PROCESS(thread1, "thread1");
PROCESS(thread2, "thread2");
PROCESS(thread3, "thread3");

// globals
int tid;

typedef enum {
    SLEEP,
    RECEIVE,
    SEND,
} Syscall;

typedef struct {
    union {
        ec_frame_tc_sleep_t* sleep;
        ec_frame_tc_receive_t* receive;
        ec_frame_tc_send_t* send;
    } frames;
    Syscall syscall;
} ThreadContext;

ThreadContext threads[3];

// blocking

void tc_sleep(ec_frame_tc_sleep_t* frame)
{
    threads[tid-1].frames.sleep = frame;
    threads[tid-1].syscall = SLEEP;
}

void tc_receive(ec_frame_tc_receive_t* frame)
{
    threads[tid-1].frames.receive = frame;
    threads[tid-1].syscall = RECEIVE;
}

void tc_send(ec_frame_tc_send_t* frame)
{
    threads[tid-1].frames.send = frame;
    threads[tid-1].syscall = SEND;
}

// syscalls

#define SYSCALL_SLEEP(thread_id) \
    static ec_frame_tc_sleep_t* frame = 0; \
    frame = threads[thread_id-1].frames.sleep; \
    static struct etimer et; \
    etimer_set(&et, frame->tics); \
    PROCESS_WAIT_UNTIL(etimer_expired(&et)); \
    continuation = frame->ec_cont; \
    frame->ec_result = SUCCESS;

#define SYSCALL_RECEIVE(thread_id) \
    static ec_frame_tc_receive_t* frame = 0;\
    frame = threads[thread_id-1].frames.receive; \
    do {\
        PROCESS_YIELD();\
    } while(!(ev == tcpip_event && uip_newdata()));\
    if (frame->buflen < uip_datalen()) {\
        *frame->len = frame->buflen;\
    } else {\
        *frame->len = uip_datalen();\
    }\
    memcpy(frame->buffer, uip_appdata, *frame->len); \
    continuation = frame->ec_cont;\
    frame->ec_result = SUCCESS;

#define SYSCALL_SEND(thread_id) \
    static ec_frame_tc_send_t* frame = 0;\
    frame = threads[thread_id-1].frames.send; \
    uip_udp_packet_sendto(frame->conn, frame->buffer, frame->len, frame->addr, frame->rport);\
    PROCESS_PAUSE();\
    continuation = frame->ec_cont;\
    frame->ec_result = SUCCESS;

// threads
PROCESS_THREAD(thread1, ev, data) // collect
{

    PROCESS_BEGIN();
    PROCESS_PAUSE();

    static void* continuation = 0;
    while(true) {
        tid = 1;
        ec_thread_1(continuation);
        if (threads[1-1].syscall == SLEEP) {
            SYSCALL_SLEEP(1);
        } else {
            printf("thread 1: unexpected syscall!\n");
        }
    }

    PROCESS_END();
}

PROCESS_THREAD(thread2, ev, data)
{
    PROCESS_BEGIN();
    PROCESS_PAUSE();

    static void* continuation = 0;
    while(true) {
        tid = 2;
        ec_thread_2(continuation);
        if (threads[2-1].syscall == RECEIVE) {
            SYSCALL_RECEIVE(2);
        } else {
            printf("thread 2: unexpected syscall!\n");
        }
    }

    PROCESS_END();    
}

PROCESS_THREAD(thread3, ev, data)
{
    PROCESS_BEGIN();
    PROCESS_PAUSE();

    static void* continuation = 0;
    while(true) {
        tid = 3;
        ec_thread_3(continuation);
        if (threads[3-1].syscall == SEND) {
            SYSCALL_SEND(3);
        } else if (threads[3-1].syscall == SLEEP) {
            SYSCALL_SLEEP(3);
        } else {
            printf("thread 3: unexpected syscall\n");
        }
    }

    PROCESS_END();
}


AUTOSTART_PROCESSES(&thread1, &thread2, &thread3);
