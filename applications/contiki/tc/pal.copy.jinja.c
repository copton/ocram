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

int tid;
typedef enum {
/*{ for syscall in all_syscalls }*/
    SYSCALL_/*syscall*/,
/*{ endfor }*/
} Syscall;


/*{ for syscall in all_syscalls }*/
typedef struct {
    ec_frame_/*syscall*/_t* frame;
/*{ if syscall == "tc_sleep" }*/
    struct etimer et;
/*{ endif }*/
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

/*{ if "tc_sleep" in all_syscalls }*/
void tc_sleep(ec_frame_tc_sleep_t* frame) {
    ThreadContext* thread = threads + tid;
    thread->ctx.tc_sleep.frame = frame;
    thread->syscall = SYSCALL_tc_sleep;

    if (frame->cond) {
        OBS("sleep: tics=%ld, cond.waiting=%d", frame->tics, frame->cond->waiting);
        frame->cond->waiting = true;
        frame->cond->waiting_process = PROCESS_CURRENT();
    } else {
        OBS("sleep: tics=%ld", frame->tics);
    }

    clock_time_t now = clock_time();
    if (frame->tics > now) {
        etimer_set(&thread->ctx.tc_sleep.et, frame->tics - now);
    } else {
        process_post(PROCESS_CURRENT(), PROCESS_EVENT_CONTINUE, &tc_sleep);
    }
}
/*{ endif }*/

/*{ if "tc_receive" in all_syscalls }*/
void tc_receive(ec_frame_tc_receive_t* frame) {
    ThreadContext* thread = threads + tid;
    thread->ctx.tc_receive.frame = frame;
    thread->syscall = SYSCALL_tc_receive;

    if (frame->cond) {
        OBS("receive: cond.waiting=%d", frame->cond->waiting);
        frame->cond->waiting = true;
        frame->cond->waiting_process = PROCESS_CURRENT();
    } else {
        OBS("receive: %s", "-");
    }
}
/*{ endif }*/

/*{ if "tc_condition_wait" in all_syscalls }*/
void tc_condition_wait(ec_frame_tc_condition_wait_t* frame) {
    ThreadContext* thread = threads + tid;
    thread->ctx.tc_condition_wait.frame = frame;
    thread->syscall = SYSCALL_tc_receive;

    OBS("condition_wait: cond.waiting=%d", frame->cond->waiting);
    frame->cond->waiting_process = PROCESS_CURRENT();
    frame->cond->waiting = true;
/*{ endif }*/

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
void ec_thread_/*loop.index0*/(void*);
static char event_handler_thread/*loop.index0*/(struct pt *process_pt, process_event_t ev, process_data_t data)
{
    PROCESS_BEGIN();

    static void* continuation = 0;
    while(true) {
        tid = /*loop.index0*/;
        ec_thread_/*loop.index0*/(continuation);
        PROCESS_YIELD();
        if (0) { }
/*{ if "tc_sleep" in syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_sleep) {
            if (0) {
            } else if (ev == PROCESS_EVENT_TIMER && data == &threads[/*loop.index0*/].ctx.tc_sleep.et) {
                threads[/*loop.index0*/].ctx.tc_sleep.frame->ec_result = false;
            } else if (ev == PROCESS_EVENT_CONTINUE
                    && data == &tc_condition_signal
                    && threads[/*loop.index0*/].ctx.tc_sleep.frame->cond != NULL
                    && threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting == false) {
                threads[/*loop.index0*/].ctx.tc_sleep.frame->ec_result = true;
                etimer_stop(&threads[/*loop.index0*/].ctx.tc_sleep.et);
            } else if (ev == PROCESS_EVENT_CONTINUE && data == &tc_sleep) {
                threads[/*loop.index0*/].ctx.tc_sleep.frame->ec_result = false;
            } else {
                ASSERT(false);
            }
            if (threads[/*loop.index0*/].ctx.tc_sleep.frame->cond) {
                threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting = false;
            }
            continuation = threads[/*loop.index0*/].ctx.tc_sleep.frame->ec_cont;
        }
/*{ endif }*/ 
/*{ if "tc_receive" in syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_receive) {
            if (0) {
            } else if (ev == tcpip_event && uip_newdata()) {
                threads[/*loop.index0*/].ctx.tc_receive.frame->ec_result = false;
            } else if (ev == PROCESS_EVENT_CONTINUE
                    && data == &tc_condition_signal
                    && threads[/*loop.index0*/].ctx.tc_sleep.frame->cond != NULL
                    && threads[/*loop.index0*/].ctx.tc_sleep.frame->cond->waiting == false) {
                threads[/*loop.index0*/].ctx.tc_receive.frame->ec_result = true;
            } else {
                ASSERT(false);
            }   
            if (threads[/*loop.index0*/].ctx.tc_receive.frame->cond) {
                threads[/*loop.index0*/].ctx.tc_receive.frame->cond->waiting = false;
            }
            continuation = threads[/*loop.index0*/].ctx.tc_receive.frame->ec_cont;
        }
/*{ endif }*/
/*{ if "tc_condition_wait" in syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_condition_wait) {
            if (0) {
            } else if (ev == PROCESS_EVENT_CONTINUE
                    && data == &tc_condition_signal
                    && threads[/*loop.index0*/].ctx.tc_condition_wait.frame->cond->waiting == false) {
            } else {
                ASSERT(false);
            }
            continuation = threads[/*loop.index0*/].ctx.tc_condition_wait.frame->ec_cont;
        }
/*{ endif }*/
        else {
            ASSERT(false);
        }
    }
    PROCESS_END();
}

struct process thread/*loop.index0*/ = { ((void *)0), "thread/*loop.index0*/", event_handler_thread/*loop.index0*/};
/*{ endfor }*/

AUTOSTART_PROCESSES(/*{ for _ in thread_syscalls }*/&thread/*loop.index0*/ /*{ if not loop.last }*/, /*{ endif }*//*{ endfor }*/);

