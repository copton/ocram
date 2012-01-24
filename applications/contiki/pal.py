#!/usr/bin/env python

import sys

number_of_threads = len(sys.argv) - 1
assert number_of_threads > 0, "at leat one thread must be started"

out = sys.stdout.write

class Syscalls(object):
    @staticmethod
    def tc_sleep(thread_id):
        out("""
            else if (threads[%(thread_id)d].syscall == SLEEP) {
                static ec_frame_tc_sleep_t* frame = 0; 
                frame = threads[%(thread_id)d].frames.sleep; 
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

    @staticmethod
    def tc_receive(thread_id):
        out("""
            else if (threads[%(thread_id)d].syscall == RECEIVE) {
                static ec_frame_tc_receive_t* frame = 0;
                frame = threads[%(thread_id)d].frames.receive;
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

    @staticmethod
    def tc_send(thread_id):
        out("""
            else if (threads[%(thread_id)d].syscall == SEND) {
                static ec_frame_tc_send_t* frame = 0;
                frame = threads[%(thread_id)d].frames.send;
                uip_udp_packet_sendto(frame->conn, frame->buffer, frame->len, frame->addr, frame->rport);
                PROCESS_PAUSE();
                continuation = frame->ec_cont;
                frame->ec_result = SUCCESS;
            }
        """ % {'thread_id' : thread_id})

out("""
#include "contiki.h"
#include "clock.h"
#include "contiki-net.h"
#include "net/netstack.h"
#include "net/uip.h"
#include "net/uiplib.h"
#include "types.h"

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

void tc_sleep(ec_frame_tc_sleep_t* frame)
{
    threads[tid].frames.sleep = frame;
    threads[tid].syscall = SLEEP;
}

void tc_receive(ec_frame_tc_receive_t* frame)
{
    threads[tid].frames.receive = frame;
    threads[tid].syscall = RECEIVE;
}

void tc_send(ec_frame_tc_send_t* frame)
{
    threads[tid].frames.send = frame;
    threads[tid].syscall = SEND;
}
""" % {'number_of_threads': number_of_threads})

def prolog(thread_id):
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

def epilog(thread_id):
    out("""
        else {
            printf("thread %(thread_id)d: unexpected syscall!\\n");
        }
    }

    PROCESS_END();
}
""" % {'thread_id': thread_id})

for thread_id, syscalls in enumerate(sys.argv[1:]):
    prolog(thread_id)
    for syscall in syscalls.split(','):
        try:
            f = getattr(Syscalls, syscall)
        except AttributeError:
            assert False, "unknown syscall " + syscall
        f(thread_id)
    epilog(thread_id)

out("AUTOSTART_PROCESSES(")
out(", ".join(["&thread%d" % tid for tid in range(number_of_threads)]))
out(");\n")
