#include <stdbool.h>

#include "contiki.h"
#include "cooja.h"

#include "tc/condition.h"
/*{ if "tc_sleep" in all_syscalls }*/
#include "clock.h"
/*{ endif }*/
/*{ if "tc_receive" in all_syscalls }*/
#include "contiki-net.h"
#include "net/uip.h"
#include "net/uiplib.h"
/*{ endif }*/

/* pal_code */
/*{ for _ in thread_syscalls }*/
void ec_thread_/*loop.index0*/(void*);
PROCESS(thread/*loop.index0*/, "thread/*loop.index0*/");
/*{ endfor }*/

typedef enum {
/*{ for syscall in all_syscalls }*/
    SYSCALL_/*syscall*/,
/*{ endfor }*/
} Syscall;

typedef struct {
    union {
/*{ for syscall in all_syscalls }*/
    ec_frame_/*syscall*/_t* /*syscall*/;
/*{ endfor }*/
    } ctx;
    Syscall syscall;

    struct pt pt;
    void (*thread_execution_function)(void*);
    void* continuation;
/*{ if "tc_sleep" in all_syscalls }*/
    struct etimer et;
/*{ endif }*/
} ThreadContext;

ThreadContext threads[/*numberof_threads*/];
ThreadContext* current_thread;

/*{ for syscall in all_syscalls }*/
void /*syscall*/(ec_frame_/*syscall*/_t* frame) {
    current_thread->ctx./*syscall*/ = frame;
    current_thread->syscall = SYSCALL_/*syscall*/;
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

PT_THREAD(application_thread(ThreadContext* thread, process_event_t ev, void* data)) {
    PT_BEGIN(&thread->pt);
    thread->continuation = 0;
    while(true) {
        current_thread = thread;
        thread->thread_execution_function(thread->continuation);
        if (0) { }
/*{ if "tc_sleep" in all_syscalls }*/
        else if (thread->syscall == SYSCALL_tc_sleep) {
            if (thread->ctx.tc_sleep->cond) {
                OBS("sleep: tics=%ld, cond.waiting=%d", thread->ctx.tc_sleep->tics, threads[/*loop.index0*/].ctx.tc_sleep->cond->waiting);
                thread->ctx.tc_sleep->cond->waiting = true;
                thread->ctx.tc_sleep->cond->waiting_process = PROCESS_CURRENT();
            } else {
                OBS("sleep: tics=%ld", thread->ctx.tc_sleep->tics);
            }
            static struct etimer et; 
            clock_time_t now = clock_time();
            if (thread->ctx.tc_sleep->tics > now) {
                etimer_set(&et, thread->ctx.tc_sleep->tics - now);
                do {
                    PT_YIELD(&thread->pt);
                    if (ev == PROCESS_EVENT_TIMER && data == &et) {
                        thread->ctx.tc_sleep->ec_result = false;
                        break;
                    }
                    if (ev == PROCESS_EVENT_CONTINUE && data == &tc_condition_signal && thread->ctx.tc_sleep->cond != NULL && threads[/*loop.index0*/].ctx.tc_sleep->cond->waiting == false) {
                        thread->ctx.tc_sleep->ec_result = true;
                        etimer_stop(&et);
                        break;
                    }
                } while(1);
            } else {
                PT_YIELD(&thread->pt);
                thread->ctx.tc_sleep->ec_result = false;
            }
            if (thread->ctx.tc_sleep->cond) {
                thread->ctx.tc_sleep->cond->waiting = false;
            }
            thread->continuation = thread->ctx.tc_sleep->ec_cont;
        }
/*{ endif }*/ 
/*{ if "tc_receive" in all_syscalls }*/
        else if (thread->syscall == SYSCALL_tc_receive) {
            if (thread->ctx.tc_receive->cond) {
                OBS("receive: cond.waiting=%d", thread->ctx.tc_receive->cond->waiting);
                thread->ctx.tc_receive->cond->waiting = true;
                thread->ctx.tc_receive->cond->waiting_process = PROCESS_CURRENT();
            } else {
                OBS("receive: %s", "-");
            }
            do {
                PT_YIELD(&thread->pt);
                if (ev == tcpip_event && uip_newdata()) {
                    thread->ctx.tc_receive->ec_result = false;
                    break;
                }
                if (ev == PROCESS_EVENT_CONTINUE && data == &tc_condition_signal && thread->ctx.tc_sleep->cond != NULL && threads[/*loop.index0*/].ctx.tc_sleep->cond->waiting == false) {
                    thread->ctx.tc_receive->ec_result = true;
                    break;
                }
            } while(1);
            if (thread->ctx.tc_receive->cond) {
                thread->ctx.tc_receive->cond->waiting = false;
            }
            thread->continuation = thread->ctx.tc_receive->ec_cont;
        }
/*{ endif }*/
/*{ if "tc_condition_wait" in all_syscalls }*/
        else if (thread->syscall == SYSCALL_tc_condition_wait) {
            OBS("condition_wait: cond.waiting=%d", thread->ctx.tc_condition_wait->cond->waiting);
            thread->ctx.tc_condition_wait->cond->waiting_process = PROCESS_CURRENT();
            thread->ctx.tc_condition_wait->cond->waiting = true;
            PT_YIELD_UNTIL(&thread->pt, ev == PROCESS_EVENT_CONTINUE && data == &tc_condition_signal && thread->ctx.tc_condition_wait->cond->waiting == false);
            thread->continuation = thread->ctx.tc_condition_wait->ec_cont;
        }
/*{ endif }*/
        else {
            ASSERT(false);
        }
    }
    PT_END(&thread->pt);
}

/*{ for syscalls in thread_syscalls }*/
PROCESS_THREAD(thread/*loop.index0*/, ev, data)
{
    PROCESS_BEGIN();
    threads[/*loop.index0*/].thread_execution_function = &ec_thread_/*loop.index0*/;

    PT_SPAWN(process_pt, &threads[/*loop.index0*/].pt, application_thread(threads + /*loop.index0*/, ev, data));
    
    PROCESS_END();
}
/*{ endfor }*/

AUTOSTART_PROCESSES(/*{ for _ in thread_syscalls }*/&thread/*loop.index0*/ /*{ if not loop.last }*/, /*{ endif }*//*{ endfor }*/);

