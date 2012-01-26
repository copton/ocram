#!/usr/bin/env python

import sys
import itertools

number_of_threads = len(sys.argv) - 1
assert number_of_threads > 0, "at leat one thread must be started"

syscalls_per_thread = map(lambda desc: desc.split(","), sys.argv[1:])
syscalls_unique = set(itertools.chain(*syscalls_per_thread))

out = sys.stdout.write

def dep_out(syscalls, string):
    if not type(syscalls) == list:
        syscalls = [syscalls]
    for syscall in syscalls:
        if syscall in syscalls_unique:
            out(string)
            return

def all_sys_out(string):
    out("\n")
    out("\n".join([string % {'syscall': syscall} for syscall in syscalls_unique]))

def all_threads_out(string):
    out("\n")
    out("\n".join([string % {'tid': tid} for tid in range(number_of_threads)]))

out("""
#include "contiki.h"
#include <stdbool.h>
#include <stdio.h>
""")
dep_out("tc_sleep", '#include "clock.h"\n')
dep_out(["tc_receive", "tc_send"],"""
#include "contiki-net.h"
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include "net/uip.h"
#include "net/uiplib.h"
""")
dep_out("tc_await_button", '#include "dev/button-sensor.h"\n')

out(sys.stdin.read())

all_threads_out("void ec_thread_%(tid)d(void*);")
all_threads_out('PROCESS(thread%(tid)d, "thread%(tid)d");')

out("""
int tid;

typedef enum {""")
all_sys_out("SYSCALL_%(syscall)s,")
out("""
} Syscall;
""")

for syscall in ["tc_sleep", "tc_receive", "tc_send", "tc_await_button"]:
    dep_out(syscall, """
typedef struct {
    ec_frame_%(syscall)s_t* frame;
} ec_ctx_%(syscall)s_t;
""" % locals())

dep_out("tc_coap_send_transaction", """
typedef struct {
    ec_frame_tc_coap_send_transaction* frame;
    coap_packet_t* response;
    struct process* process;
} ec_ctx_tc_coap_send_transaction_t;
""")

out("""
typedef struct {
    union {""")
all_sys_out("\t\tec_ctx_%(syscall)s_t %(syscall)s;")
out("""
    } ctx;
    Syscall syscall;
} ThreadContext;

ThreadContext threads[%(number_of_threads)d];
""" % locals())

dep_out("tc_sleep", """
void tc_sleep(ec_frame_tc_sleep_t* frame)
{
    threads[tid].ctx.tc_sleep.frame = frame;
    threads[tid].syscall = SYSCALL_tc_sleep;
}
""")

dep_out("tc_receive", """
void tc_receive(ec_frame_tc_receive_t* frame)
{
    threads[tid].ctx.tc_receive.frame = frame;
    threads[tid].syscall = SYSCALL_tc_receive;
}
""")

dep_out("tc_send", """
void tc_send(ec_frame_tc_send_t* frame)
{
    threads[tid].ctx.tc_send.frame = frame;
    threads[tid].syscall = SYSCALL_tc_send;
}
""")

dep_out("tc_await_button", """
void tc_await_button(ec_frame_tc_await_button_t* frame)
{
    threads[tid].ctx.tc_await_button.frame = frame;
    threads[tid].syscall = SYSCALL_tc_await_button;
}
""")

dep_out("tc_coap_send_transaction", """
void tc_coap_send_transaction(ec_frame_tc_coap_send_transaction* frame)
{
    threads[tid].ctx.tc_coap_send_transaction = frame;
    threads[tid].syscall = SYSCALL_tc_coap_send_transaction;
}

void coap_transaction_callback(void* data, void* response)
{
    ec_ctx_tc_coap_send_transaction_t* ctx = data;
    ctx->response = response;
    process_poll(ctx->process);
}
""")

for thread_id, syscalls_of_thread in enumerate(syscalls_per_thread):
    out("""
PROCESS_THREAD(thread%(thread_id)d, ev, data)
{

    PROCESS_BEGIN();
    PROCESS_PAUSE();

    static void* continuation = 0;
    while(true) {
        tid = %(thread_id)d;
        ec_thread_%(thread_id)d(continuation);
        if (0) { }
""" % locals())
    
    dep_out("tc_sleep", """
        else if (threads[%(thread_id)d].syscall == SYSCALL_tc_sleep) {
            static ec_frame_tc_sleep_t* frame = 0; 
            frame = threads[%(thread_id)d].ctx.tc_sleep.frame;
            static struct etimer et; 
            clock_time_t now = clock_time();
            if (frame->tics > now) {
                etimer_set(&et, frame->tics - now);
                PROCESS_WAIT_UNTIL(etimer_expired(&et));
            } else {
                PROCESS_PAUSE();
            }
            continuation = frame->ec_cont;
        }
""" % locals())

    dep_out("tc_receive", """
        else if (threads[%(thread_id)d].syscall == SYSCALL_tc_receive) {
            static ec_frame_tc_receive_t* frame = 0;
            frame = threads[%(thread_id)d].ctx.tc_receive.frame;
            PROCESS_YIELD_UNTIL(ev == tcpip_event && uip_newdata());
            if (frame->buflen < uip_datalen()) {
                *frame->len = frame->buflen;
            } else {
                *frame->len = uip_datalen();
            }
            memcpy(frame->buffer, uip_appdata, *frame->len);
            continuation = frame->ec_cont;
        }
""" % locals())
   
    dep_out("tc_send", """
        else if (threads[%(thread_id)d].syscall == SYSCALL_tc_send) {
            static ec_frame_tc_send_t* frame = 0;
            frame = threads[%(thread_id)d].ctx.tc_send.frame;
            uip_udp_packet_sendto(frame->conn, frame->buffer, frame->len, frame->addr, frame->rport);
            PROCESS_PAUSE();
            continuation = frame->ec_cont;
        }
""" % locals())

    dep_out("tc_await_button", """
        else if (threads[%(thread_id)d].syscall == SYSCALL_tc_await_button) {
            static ec_frame_tc_await_button_t* frame = 0;
            frame = threads[%(thread_id)d].ctx.tc_await_button.frame;
            PROCESS_YIELD_UNTIL(ev == sensors_event && data == &button_sensor);
            continuation = frame->ec_cont;
        }
""" % locals())

    dep_out("tc_coap_send_transaction", """
        else if (threads[%(thread_id)d].syscall == SYSCALL_tc_coap_send_transaction) {
            static ec_ctx_tc_coap_send_transaction_t* ctx = 0;
            ctx = threads[%(thread_id)d].ctx.tc_coap_send_transaction;
            ctx.frame->transaction->process = PROCESS_CURRENT();
            ctx.frame->transaction->callback = &coap_transaction_callback;
            ctx.frame->transaction->callback_data = ctx;
            PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);
            ctx.frame->ec_result = ctx.response;
            continuation = ctx.frame->ec_cont;
        }
""" % locals())

    out("""
        else {
            printf("thread %(thread_id)d: unexpected syscall!\\n");
        }
    }

    PROCESS_END();
}
""" % {'thread_id': thread_id})

out("AUTOSTART_PROCESSES(")
out(", ".join(["&thread%d" % tid for tid in range(number_of_threads)]))
out(");\n")
