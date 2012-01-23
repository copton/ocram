#!/usr/bin/env python

import sys

if len(sys.argv) <= 1:
    sys.stderr.write("usage: %s SLEEP,RECEIVE,SEND ...\n" % sys.argv[0])
    sys.exit(1)

number_of_threads = len(sys.argv) - 1
assert number_of_threads > 0, "at leat one thread must be started"

out = sys.stdout.write

def sleep(thread_id):
    return """
        else if (threads[%(thread_id)d-1].syscall == SLEEP) {
            static ec_frame_tc_sleep_t* frame = 0; 
            frame = threads[%(thread_id)d-1].frames.sleep; 
            static struct etimer et; 
            clock_time_t now = clock_time();
            if (now > frame->tics) {
                etimer_set(&et, now - frame->tics);
                PROCESS_WAIT_UNTIL(etimer_expired(&et));
            } else {
                PROCESS_PAUSE();
            }
            continuation = frame->ec_cont;
            frame->ec_result = SUCCESS;
        }
    """ % {'thread_id' : thread_id}

def receive(thread_id):
    return """
        else if (threads[%(thread_id)d-1].syscall == RECEIVE) {
            static ec_frame_tc_receive_t* frame = 0;
            frame = threads[%(thread_id)d-1].frames.receive;
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
    """ % {'thread_id' : thread_id}

def send(thread_id):
    return """
        else if (threads[%(thread_id)d-1].syscall == SEND) {
            static ec_frame_tc_send_t* frame = 0;
            frame = threads[%(thread_id)d-1].frames.send;
            uip_udp_packet_sendto(frame->conn, frame->buffer, frame->len, frame->addr, frame->rport);
            PROCESS_PAUSE();
            continuation = frame->ec_cont;
            frame->ec_result = SUCCESS;
        }
    """ % {'thread_id' : thread_id}

def header(number_of_threads):
    includes = """
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
"""
    fun_decls = "\n".join(["void ec_thread_%d(void*);" % (tid+1) for tid in range(number_of_threads)])
    proc_decls = "\n".join(['PROCESS(thread%(tid)d, "thread%(tid)d");' % {'tid': tid+1} for tid in range(number_of_threads)])

    context = """
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

ThreadContext threads[%(number_of_threads)d];
    """ % {'number_of_threads': number_of_threads}

    syscalls = """
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
    """

    return includes + "\n" + fun_decls + "\n" + proc_decls + context + syscalls

def footer(number_of_threads):
    return "AUTOSTART_PROCESSES(" + ", ".join(["&thread%d" % (tid+1) for tid in range(number_of_threads)]) + ");\n"

def prolog(thread_id):
    return """
PROCESS_THREAD(thread%(thread_id)d, ev, data)
{

    PROCESS_BEGIN();
    PROCESS_PAUSE();

    static void* continuation = 0;
    while(true) {
        tid = %(thread_id)d;
        ec_thread_%(thread_id)d(continuation);
        if (0) { }
    """ % {'thread_id': thread_id}

def epilog(thread_id):
    return """
        else {
            printf("thread %(thread_id)d: unexpected syscall!\\n");
        }
    }

    PROCESS_END();
}
""" % {'thread_id': thread_id}

out(header(number_of_threads))
for thread_id, syscalls in enumerate(sys.argv[1:]):
    out(prolog(thread_id+1))
    for syscall in syscalls.split(','):
        if syscall == "SLEEP":
            out(sleep(thread_id+1))
        elif syscall == "SEND":
            out(send(thread_id+1))
        elif syscall == "RECEIVE":
            out(receive(thread_id+1))
        else:
            assert False, "unknown syscall " + syscall
    out(epilog(thread_id+1))
out(footer(number_of_threads))
