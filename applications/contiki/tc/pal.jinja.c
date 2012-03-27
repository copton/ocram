#include "contiki.h"
#include "debug.h"
#include <stdbool.h>
#include "tc/condition.h"
/*{ if "tc_sleep" in all_syscalls }*/
#include "clock.h"
/*{ endif }*/
/*{ if "tc_receive" in all_syscalls }*/
#include "contiki-net.h"
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include "net/uip.h"
#include "net/uiplib.h"
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

void tc_condition_signal(condition_t* cond, void* data)
{
   if (! cond->waiting) return;

   cond->waiting = false;
   cond->data = data;
   process_post(cond->waiting_process, PROCESS_EVENT_CONTINUE, &tc_condition_signal);
}

void tc_condition_init(condition_t* cond)
{
    cond->waiting_process = NULL;
    cond->waiting = false;
    cond->data = NULL;
}

/*{ for syscalls in thread_syscalls }*/
PROCESS_THREAD(thread/*loop.index0*/, ev, data)
{
    PROCESS_BEGIN();

    static void* continuation = 0;
    while(true) {
        tid = /*loop.index0*/;
        ec_thread_/*loop.index0*/(continuation);
        if (0) { }
/*{ if "tc_sleep" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_sleep) {
            if (threads[/*loop.index0*/].ctx.tc_sleep.frame->cond) {
                OBS("sleep: tics=%ld, cond.waiting=%d", threads[/*loop.index0*/].ctx.tc_sleep.frame->tics, threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting);
                threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting = true;
                threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting_process = PROCESS_CURRENT();
            } else {
                OBS("sleep: tics=%ld", threads[/*loop.index0*/].ctx.tc_sleep.frame->tics);
            }
            static struct etimer et; 
            clock_time_t now = clock_time();
            if (threads[/*loop.index0*/].ctx.tc_sleep.frame->tics > now) {
                etimer_set(&et, threads[/*loop.index0*/].ctx.tc_sleep.frame->tics - now);
                do {
                    PROCESS_YIELD();
                    if (ev == PROCESS_EVENT_TIMER && data == &et) {
                        threads[/*loop.index0*/].ctx.tc_sleep.frame->ec_result = false;
                        break;
                    }
                    if (ev == PROCESS_EVENT_CONTINUE && data == &tc_condition_signal && threads[/*loop.index0*/].ctx.tc_sleep.frame->cond != NULL && threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting == false) {
                        threads[/*loop.index0*/].ctx.tc_sleep.frame->ec_result = true;
                        etimer_stop(&et);
                        break;
                    }
                } while(1);
            } else {
                PROCESS_PAUSE();
                threads[/*loop.index0*/].ctx.tc_sleep.frame->ec_result = false;
            }
            if (threads[/*loop.index0*/].ctx.tc_sleep.frame->cond) {
                threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting = false;
            }
            continuation = threads[/*loop.index0*/].ctx.tc_sleep.frame->ec_cont;
        }
/*{ endif }*/ 
/*{ if "tc_receive" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_receive) {
            if (threads[/*loop.index0*/].ctx.tc_receive.frame->cond) {
                OBS("receive: cond.waiting=%d", threads[/*loop.index0*/].ctx.tc_receive.frame->cond->waiting);
                threads[/*loop.index0*/].ctx.tc_receive.frame->cond->waiting = true;
                threads[/*loop.index0*/].ctx.tc_receive.frame->cond->waiting_process = PROCESS_CURRENT();
            } else {
                OBS("receive");
            }
            do {
                PROCESS_YIELD();
                if (ev == tcpip_event && uip_newdata()) {
                    threads[/*loop.index0*/].ctx.tc_receive.frame->ec_result = false;
                    break;
                }
                if (ev == PROCESS_EVENT_CONTINUE && data == &tc_condition_signal && threads[/*loop.index0*/].ctx.tc_sleep.frame->cond != NULL && threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting == false) {
                    threads[/*loop.index0*/].ctx.tc_receive.frame->ec_result = true;
                    break;
                }
            } while(1);
            if (threads[/*loop.index0*/].ctx.tc_receive.frame->cond) {
                threads[/*loop.index0*/].ctx.tc_receive.frame->cond->waiting = false;
            }
            continuation = threads[/*loop.index0*/].ctx.tc_receive.frame->ec_cont;
        }
/*{ endif }*/
/*{ if "tc_condition_wait" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_condition_wait) {
            OBS("condition_wait: cond.waiting=%d", threads[/*loop.index0*/].ctx.tc_condition_wait.frame->cond->waiting);
            threads[/*loop.index0*/].ctx.tc_condition_wait.frame->cond->waiting_process = PROCESS_CURRENT();
            threads[/*loop.index0*/].ctx.tc_condition_wait.frame->cond->waiting = true;
            PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_CONTINUE && data == &tc_condition_signal && threads[/*loop.index0*/].ctx.tc_condition_wait.frame->cond->waiting == false);
            continuation = threads[/*loop.index0*/].ctx.tc_condition_wait.frame->ec_cont;
        }
/*{ endif }*/
        else {
            ASSERT(false);
        }
    }
    PROCESS_END();
}
/*{ endfor }*/

AUTOSTART_PROCESSES(/*{ for _ in thread_syscalls }*/&thread/*loop.index0*/ /*{ if not loop.last }*/, /*{ endif }*//*{ endfor }*/);

