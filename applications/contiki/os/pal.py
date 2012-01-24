#!/usr/bin/env python

import sys
import itertools

number_of_threads = len(sys.argv) - 1
assert number_of_threads > 0, "at leat one thread must be started"

out = sys.stdout.write

syscalls_per_thread = map(lambda desc: desc.split(","), sys.argv[1:])
syscalls_unique = set(itertools.chain(*syscalls_per_thread))

out("""
#include "contiki.h"
#include "clock.h"
#include "contiki-net.h"
#include "net/netstack.h"
#include "net/uip.h"
#include "net/uiplib.h"
#include "../../../os/types.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
""")

out(sys.stdin.read())

out("\n".join(["void ec_thread_%d(void*);" % tid for tid in range(number_of_threads)]))
out("\n")
out("\n".join(['PROCESS(thread%(tid)d, "thread%(tid)d");' % {'tid': tid} for tid in range(number_of_threads)]))

out("""
int tid;

typedef enum {
""")
out(",\n".join(["SYSCALL_" + syscall for syscall in syscalls_unique]))
out("""
} Syscall;

typedef struct {
    union {
""")
out("\n".join(["ec_frame_%(syscall)s_t* %(syscall)s;" % {'syscall': syscall} for syscall in syscalls_unique]))
out("""
    } frames;
    Syscall syscall;
} ThreadContext;

ThreadContext threads[%(number_of_threads)d];
""" % {'number_of_threads': number_of_threads})

if "tc_sleep" in syscalls_unique:
    out("""
void tc_sleep(ec_frame_tc_sleep_t* frame)
{
    threads[tid].frames.tc_sleep = frame;
    threads[tid].syscall = SYSCALL_tc_sleep;
}
""")

if "tc_receive" in syscalls_unique:
    out("""
void tc_receive(ec_frame_tc_receive_t* frame)
{
    threads[tid].frames.tc_receive = frame;
    threads[tid].syscall = SYSCALL_tc_receive;
}
""")

if "tc_send" in syscalls_unique:
    out("""
void tc_send(ec_frame_tc_send_t* frame)
{
    threads[tid].frames.tc_send = frame;
    threads[tid].syscall = SYSCALL_tc_send;
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
""" % {'thread_id': thread_id})
    
    if "tc_sleep" in syscalls_of_thread:
        out("""
            else if (threads[%(thread_id)d].syscall == SYSCALL_tc_sleep) {
                static ec_frame_tc_sleep_t* frame = 0; 
                frame = threads[%(thread_id)d].frames.tc_sleep; 
                static struct etimer et; 
                clock_time_t now = clock_time();
                if (frame->tics > now) {
                    etimer_set(&et, frame->tics - now);
                    PROCESS_WAIT_UNTIL(etimer_expired(&et));
                } else {
                    PROCESS_PAUSE();
                }
                continuation = frame->ec_cont;
                frame->ec_result = SUCCESS;
            }
""" % {'thread_id' : thread_id})

    if "tc_receive" in syscalls_of_thread:
        out("""
            else if (threads[%(thread_id)d].syscall == SYSCALL_tc_receive) {
                static ec_frame_tc_receive_t* frame = 0;
                frame = threads[%(thread_id)d].frames.tc_receive;
                do {
                    PROCESS_YIELD();
                } while(!(ev == tcpip_event && uip_newdata()));
                if (frame->buflen < uip_datalen()) {
                    *frame->len = frame->buflen;
                } else {
                    *frame->len = uip_datalen();
                }
                memcpy(frame->buffer, uip_appdata, *frame->len);
                continuation = frame->ec_cont;
                frame->ec_result = SUCCESS;
            }
""" % {'thread_id' : thread_id})
   
    if "tc_send" in syscalls_of_thread:
        out("""
            else if (threads[%(thread_id)d].syscall == SYSCALL_tc_send) {
                static ec_frame_tc_send_t* frame = 0;
                frame = threads[%(thread_id)d].frames.tc_send;
                uip_udp_packet_sendto(frame->conn, frame->buffer, frame->len, frame->addr, frame->rport);
                PROCESS_PAUSE();
                continuation = frame->ec_cont;
                frame->ec_result = SUCCESS;
            }
""" % {'thread_id' : thread_id})

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
