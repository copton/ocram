#include "contiki.h"
#include <stdbool.h>
#include <stdio.h>
/*{ if "tc_sleep" in all_syscalls }*/
#include "clock.h"
/*{ endif }*/
/*{ if "tc_send" in all_syscalls or "tc_receive" in all_syscalls }*/
#include "contiki-net.h"
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include "net/uip.h"
#include "net/uiplib.h"
/*{ endif }*/
/*{ if "tc_await_button" in all_syscalls }*/
#include "dev/button-sensor.h"
/*{ endif }*/
/*{ if (("tc_condition_wait" in all_syscalls) or ("tc_condition_time_wait" in all_syscalls)) }*/
#include "os/condition.h"
/*{ endif }*/

/* pal_code */
/*{ for _ in thread_syscalls }*/
void ec_thread_/*loop.index0*/(void*);
PROCESS(thread/*loop.index0*/, "thread/*loop.index0*/");
/*{ endfor }*/

int tid;
typedef enum {
/*{ for syscall in all_syscalls }*/
    SYSCALL_/*syscall*/,
/*{ endfor }*/
} Syscall;

/*{ for syscall in all_syscalls }*/
typedef struct {
    ec_frame_/*syscall*/_t* frame;
} ec_ctx_/*syscall*/_t;
/*{ endfor }*/
 
typedef struct {
    union {
/*{ for syscall in all_syscalls }*/
        ec_ctx_/*syscall*/_t /*syscall*/;
/*{ endfor }*/
    } ctx;
    Syscall syscall;
} ThreadContext;

ThreadContext threads[/*numberof_threads*/];

/*{ for syscall in all_syscalls }*/
void /*syscall*/(ec_frame_/*syscall*/_t* frame) {
    threads[tid].ctx./*syscall*/.frame = frame;
    threads[tid].syscall = SYSCALL_/*syscall*/;
}
/*{ endfor }*/

/*{ if "tc_condition_wait" in all_syscalls or "tc_condition_time_wait" in all_syscalls}*/
void tc_condition_signal(condition_t* cond)
{
    if (cond->waiting) {
       cond->waiting = false;
       process_post(cond->p, PROCESS_EVENT_CONTINUE, NULL);
    }
}
/*{ endif }*/

/*{ for syscalls in thread_syscalls }*/
PROCESS_THREAD(thread/*loop.index0*/, ev, data)
{
    PROCESS_BEGIN();
    printf("thread address: /*loop.index0*/: %p\n", process_current);

    static void* continuation = 0;
    while(true) {
        tid = /*loop.index0*/;
        ec_thread_/*loop.index0*/(continuation);
        if (0) { }
/*{ if "tc_sleep" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_sleep) {
            ec_frame_tc_sleep_t* frame = threads[/*loop.index0*/].ctx.tc_sleep.frame;
            static struct etimer et; 
            clock_time_t now = clock_time();
            if (frame->tics > now) {
                etimer_set(&et, frame->tics - now);
                PROCESS_YIELD_UNTIL(etimer_expired(&et));
            } else {
                PROCESS_PAUSE();
            }
            frame = threads[/*loop.index0*/].ctx.tc_sleep.frame;
            continuation = frame->ec_cont;
        }
/*{ endif }*/ 
/*{ if "tc_receive" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_receive) {
            ec_frame_tc_receive_t* frame = threads[/*loop.index0*/].ctx.tc_receive.frame;
            PROCESS_YIELD_UNTIL(ev == tcpip_event && uip_newdata());
            continuation = frame->ec_cont;
        }
/*{ endif }*/
/*{ if "tc_send" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_send) {
            ec_frame_tc_send_t* frame = threads[/*loop.index0*/].ctx.tc_send.frame;
            uip_udp_packet_sendto(frame->conn, frame->buffer, frame->len, frame->addr, frame->rport);
            PROCESS_PAUSE();
            frame = threads[/*loop.index0*/].ctx.tc_send.frame;
            continuation = frame->ec_cont;
        }
/*{ endif }*/
/*{ if "tc_await_button" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_await_button) {
            ec_frame_tc_await_button_t* frame = threads[/*loop.index0*/].ctx.tc_await_button.frame;
            PROCESS_YIELD_UNTIL(ev == sensors_event && data == &button_sensor);
            frame = threads[/*loop.index0*/].ctx.tc_await_button.frame;
            continuation = frame->ec_cont;
        }
/*{ endif }*/
/*{ if "tc_condition_wait" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_condition_wait) {
            ec_frame_tc_condition_wait* frame = threads[/*loop.index0*/].ctx.tc_condition_wait.frame;
            frame->cond->waiting_process = PROCESS_CURRENT();
            frame->cond->waiting = true;
            PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_CONTINUE && threads[/*loop.index0*/].ctx.tc_condition_wait.frame->cond->waiting == false);
            continuation = threads[/*loop.index0*/].ctx.tc_await_button.frame->ec_cont;
        }
/*{ endif }*/
/*{ if "tc_condition_time_wait" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_condition_time_wait) {
            ec_frame_tc_condition_time_wait* frame = threads[/*loop.index0*/].ctx.tc_condition_time_wait.frame;
            static struct etimer et;
            clock_time_t now = clock_time();
            if (frame->tics > now) {
                frame->cond->waiting_process = PROCESS_CURRENT();
                frame->cond->waiting = true;
                etimer_set(&et, frame->tics - now);
                PROCESS_YIELD_UNTIL((ev == PROCESS_EVENT_CONTINUE && threads[/*loop.index0*/].ctx.tc_condition_time_wait.frame->waiting == false) || etimer_expired(&et));
            } else {
                PROCESS_PAUSE();
            }
            cotinuation = threads[/*loop.index0*/].ctx.tc_condition_time_wait.frame->ec_cont;
        }
/*{ endif }*/

        else {
            printf("thread /*loop.index0*/: unexpected syscall!\\n");
        }
    }
    PROCESS_END();
}
/*{ endfor }*/

AUTOSTART_PROCESSES(/*{ for _ in thread_syscalls }*/&thread/*loop.index0*/ /*{ if not loop.last }*/, /*{ endif }*//*{ endfor }*/);

