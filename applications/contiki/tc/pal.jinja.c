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
ThreadContext* thread;

/*{ if "tc_sleep" in all_syscalls }*/
void tc_sleep(ec_frame_tc_sleep_t* frame) {
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
    thread->ctx.tc_condition_wait.frame = frame;
    thread->syscall = SYSCALL_tc_condition_wait;

    OBS("condition_wait: cond.waiting=%d", frame->cond->waiting);
    frame->cond->waiting_process = PROCESS_CURRENT();
    frame->cond->waiting = true;
}
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

static void* event_handler(process_event_t ev, process_data_t data)
{
    if (0) { }
/*{ if "tc_sleep" in all_syscalls }*/
    else if (thread->syscall == SYSCALL_tc_sleep) {
        if (0) {
        } else if (ev == PROCESS_EVENT_TIMER && data == &thread->ctx.tc_sleep.et) {
            thread->ctx.tc_sleep.frame->ec_result = false;
        } else if (ev == PROCESS_EVENT_CONTINUE
                && data == &tc_condition_signal
                && thread->ctx.tc_sleep.frame->cond != NULL
                && thread->ctx.tc_sleep.frame->cond->waiting == false) {
            thread->ctx.tc_sleep.frame->ec_result = true;
            etimer_stop(&thread->ctx.tc_sleep.et);
        } else if (ev == PROCESS_EVENT_CONTINUE && data == &tc_sleep) {
            thread->ctx.tc_sleep.frame->ec_result = false;
        } else {
            ASSERT(false);
        }
        if (thread->ctx.tc_sleep.frame->cond) {
            thread->ctx.tc_sleep.frame->cond->waiting = false;
        }
        return thread->ctx.tc_sleep.frame->ec_cont;
    }
/*{ endif }*/ 
/*{ if "tc_receive" in all_syscalls }*/
    else if (thread->syscall == SYSCALL_tc_receive) {
        if (0) {
        } else if (ev == tcpip_event && uip_newdata()) {
            thread->ctx.tc_receive.frame->ec_result = false;
        } else if (ev == PROCESS_EVENT_CONTINUE
                && data == &tc_condition_signal
                && thread->ctx.tc_sleep.frame->cond != NULL
                && thread->ctx.tc_sleep.frame->cond->waiting == false) {
            thread->ctx.tc_receive.frame->ec_result = true;
        } else {
            ASSERT(false);
        }   
        if (thread->ctx.tc_receive.frame->cond) {
            thread->ctx.tc_receive.frame->cond->waiting = false;
        }
        return thread->ctx.tc_receive.frame->ec_cont;
    }
/*{ endif }*/
/*{ if "tc_condition_wait" in all_syscalls }*/
    else if (thread->syscall == SYSCALL_tc_condition_wait) {
        if (0) {
        } else if (ev == PROCESS_EVENT_CONTINUE
                && data == &tc_condition_signal
                && thread->ctx.tc_condition_wait.frame->cond->waiting == false) {
        } else {
            ASSERT(false);
        }
        return thread->ctx.tc_condition_wait.frame->ec_cont;
    }
/*{ endif }*/
    else {
        ASSERT(false);
        return (void*) -1;
    }
}

/*{ for syscalls in thread_syscalls }*/
void ec_thread_/*loop.index0*/(void*);
static char event_handler_thread/*loop.index0*/(struct pt* process_pt, process_event_t ev, process_data_t data)
{
    thread = threads + /*loop.index0*/;
    void* continuation;
    if (process_pt->lc == 0) {
        process_pt->lc = 1;
        continuation = 0;
    } else {
        continuation = event_handler(ev, data);
    }
    ec_thread_/*loop.index0*/(continuation);
    return PT_YIELDED;
}

struct process thread/*loop.index0*/ = { ((void *)0), "thread/*loop.index0*/", event_handler_thread/*loop.index0*/};
/*{ endfor }*/

AUTOSTART_PROCESSES(/*{ for _ in thread_syscalls }*/&thread/*loop.index0*/ /*{ if not loop.last }*/, /*{ endif }*//*{ endfor }*/);


