// the following code is heavily based on TinyThreads (http://selab.csuohio.edu/tinythread/)
// here is their copyright notice
/* 
 * Copyright (c) 2006, Cleveland State University
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Cleveland State University nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' 
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE # OF THIS SOFTWARE, EVEN IF ADVISED OF 
 * THE POSSIBILITY OF SUCH DAMAGE.
 */


#include "debug.h"

#include <stdint.h>
#include "contiki.h"
#include "clock.h"
#include "tl/tl.h"

#define PUSH_GPR()         \
  __asm__("push r4");     \
  __asm__("push r5");     \
  __asm__("push r6");     \
  __asm__("push r7");     \
  __asm__("push r8");     \
  __asm__("push r9");     \
  __asm__("push r10");    \
  __asm__("push r11");    \
  __asm__("push r12");    \
  __asm__("push r13");    \
  __asm__("push r14");    \
  __asm__("push r15")

#define PUSH_PC()          \
  __asm__("push r0")

#define PUSH_STATUS()      \
  __asm__("push r2")

#define POP_PC()          \
  __asm__("pop r0")

#define POP_STATUS()      \
  __asm__("pop r2")


//Pop the general purpose registers
#define POP_GPR()         \
  __asm__("pop r15");     \
  __asm__("pop r14");     \
  __asm__("pop r13");     \
  __asm__("pop r12");     \
  __asm__("pop r11");     \
  __asm__("pop r10");     \
  __asm__("pop r9");      \
  __asm__("pop r8");      \
  __asm__("pop r7");      \
  __asm__("pop r6");      \
  __asm__("pop r5");      \
  __asm__("pop r4") 

#define SWAP_STACK_PTR(OLD, NEW) \
  __asm__("mov.w r1,%0" : "=r" (OLD));  \
  __asm__("mov.w %0,r1" : : "m" (NEW))
 
//This must be a power of 2
#define TOSH_MAX_THREADS      4
#define TOSH_MAX_THREADS_MASK (TOSH_MAX_THREADS - 1)
typedef enum {
  STATE_NULL = 0,     // There is no thread here
  STATE_READY = 1,    // Read to be scheduled but not running
  STATE_ACTIVE = 2,   // Currently executed
  STATE_BLOCKED = 3,  // This thread is blocked due to a syscall
} State;

typedef enum {
    SYSCALL_sleep,
    SYSCALL_receive,
    SYSCALL_send,
    SYSCALL_condition_wait,
    SYSCALL_condition_time_wait,
} Syscall;

typedef struct {
    struct etimer timer;
} ctx_sleep_t;

typedef struct thread {
  uint16_t* sp;
  State state;
  union {
    void (*tp) ();
    struct {
      struct etimer timer;
    } sleep;  
    struct {
      condition_t* cond;
      struct etimer** timers;
      size_t numberofTimers; 
    } condition;
  } data;
  Syscall syscall;
} thread_t;

thread_t thread_table[TOSH_MAX_THREADS];

// A pointer to the current thread; is null when in regular tinyos thread
thread_t* current_thread;

// The tinyos stack pointer while we are executing a thread
uint16_t* old_stack_ptr = 0;

PROCESS(process_scheduler, "scheduler");
process_event_t event_cond_signal;

static void init() {
    memset(thread_table, 0, sizeof(thread_t) * TOSH_MAX_THREADS);
    current_thread = 0;
    event_cond_signal = process_alloc_event();
}

__attribute__((noinline)) static void platform_switch_to_thread() {
    PUSH_STATUS();
    PUSH_GPR();
    SWAP_STACK_PTR(old_stack_ptr, current_thread->sp);
    POP_GPR();
    POP_STATUS();
    return;
}

__attribute__((noinline)) static void yield() {
    PUSH_STATUS();
    PUSH_GPR();
    SWAP_STACK_PTR(current_thread->sp, old_stack_ptr);
    POP_GPR();
    POP_STATUS();
    return;
}

static void remove_thread() {
    current_thread->state = STATE_NULL;
    yield();
    ASSERT(false);
}

static void call_fcn_ptr(void (*tp)()){
  (*tp)();
}

__attribute__((naked)) void thread_wrapper() {
    call_fcn_ptr(current_thread->data.tp);
    remove_thread();
}

uint16_t* stack_top(uint8_t* stack_, size_t size){
    ASSERT((size % 2) == 0);
    uint16_t* stack = (uint16_t*) stack_;
    return(&(stack[(size / sizeof(uint16_t)) - 1]));
}

void tl_create_thread(void (*fcn)(), uint8_t* stack, size_t size) {
    static int tid = 0;
    ASSERT(tid < TOSH_MAX_THREADS);
    ASSERT((size % 2) == 0);
    current_thread = thread_table + tid;
    tid++;
    ASSERT(current_thread->state == STATE_NULL);
    current_thread->sp = stack_top(stack, size);
    current_thread->state = STATE_READY;
    current_thread->data.tp = fcn;

    // PREPARE STACK
    SWAP_STACK_PTR(old_stack_ptr, current_thread->sp);
    __asm__("push %0":: "r" (thread_wrapper));
    __asm__("eint");
    __asm__("push r2");
    __asm__("dint");
    {int i; for(i=0;i<12;i++) {__asm__("push r3");}}
    SWAP_STACK_PTR(current_thread->sp, old_stack_ptr);

    current_thread = 0;
    process_post(PROCESS_CURRENT(), PROCESS_EVENT_CONTINUE, NULL);
}

PROCESS_THREAD(process_scheduler, ev, data)
{
    PROCESS_BEGIN();
    static size_t thread_idx = 0;
    init();
    tl_app_main();
    while(1) {
        PROCESS_YIELD();

        size_t i;
        size_t j;
        thread_t* t;
        for (i=0; i<TOSH_MAX_THREADS; i++) {
            j = TOSH_MAX_THREADS_MASK & (thread_idx + i);
            t = thread_table + j;

            if (t->state == STATE_READY && ev == PROCESS_EVENT_CONTINUE) {
                goto schedule;
            }
            if (t->state == STATE_BLOCKED) {
                if (t->syscall == SYSCALL_sleep) {
                    if (ev == PROCESS_EVENT_TIMER && data == &t->data.sleep.timer) {
                        ASSERT(etimer_expired(&t->data.sleep.timer));
                        goto schedule;
                    } else if (ev == PROCESS_EVENT_CONTINUE && data == t) {
                        goto schedule;
                    } else if (ev == event_cond_signal && data == t) {
                        goto schedule;
                    }
                }
                if (t->syscall == SYSCALL_receive) {
                    if (ev == tcpip_event && uip_newdata()) {
                        goto schedule;
                    } else if (ev == event_cond_signal && data == t) {
                        goto schedule;
                    }
                }
                if (t->syscall == SYSCALL_send && ev == PROCESS_EVENT_CONTINUE && data == t) {
                    goto schedule;
                }
                if (t->syscall == SYSCALL_condition_wait && ev == PROCESS_EVENT_CONTINUE && data == t) {
                    ASSERT(t->data.condition.cond->waiting == false);
                    goto schedule;
                }
                if (t->syscall == SYSCALL_condition_time_wait) {
                    if (ev == PROCESS_EVENT_CONTINUE && data == t) {
                        ASSERT(t->data.condition.cond->waiting == false);
                        goto schedule;
                    } else if (ev == PROCESS_EVENT_TIMER) {
                        int i;
                        for (i=0; i<t->data.condition.numberofTimers; i++) {
                            if (data == t->data.condition.timers[i]) {
                                ASSERT(etimer_expired(t->data.condition.timers[i]));
                                goto schedule;
                            }
                        }
                    }
                }
            }
        }
        continue;

schedule:
        current_thread = t;
        current_thread->state = STATE_ACTIVE;
        platform_switch_to_thread();
        current_thread = 0;

        thread_idx++;
        thread_idx = (TOSH_MAX_THREADS_MASK & (thread_idx));
    }
    PROCESS_END();
}

AUTOSTART_PROCESSES(&process_scheduler);

bool tl_sleep(clock_time_t tics, condition_t* cond) {
    if (cond) {
        cond->waiting = true;
        cond->waiting_thread = current_thread;
    }
    clock_time_t now = clock_time();
    current_thread->state = STATE_BLOCKED;
    current_thread->syscall = SYSCALL_sleep;
    if (tics > now) {
        etimer_set(&current_thread->data.sleep.timer, tics - now);
    } else {
        process_post(PROCESS_CURRENT(), PROCESS_EVENT_CONTINUE, current_thread);
    }

    yield();

    bool result = false;
    if (cond) {
        result = cond->waiting == false;
        cond->waiting = false;
    }       
    return result;
}

bool tl_receive(condition_t* cond) {
    if (cond) {
        cond->waiting = true;
        cond->waiting_thread = current_thread;
    }
    current_thread->state = STATE_BLOCKED;
    current_thread->syscall = SYSCALL_receive;
    yield();

    bool result = false;
    if (cond) {
        result = cond->waiting == false;
        cond->waiting = false;
    }       
    return result;
}

void tl_send(struct uip_udp_conn* conn, uip_ipaddr_t* addr, uint16_t rport, uint8_t* buffer, size_t len) {
    uip_udp_packet_sendto(conn, buffer, len, addr, rport);

    current_thread->state = STATE_BLOCKED;
    current_thread->syscall = SYSCALL_send;
    process_post(PROCESS_CURRENT(), PROCESS_EVENT_CONTINUE, current_thread);
    yield();
}

void tl_condition_init(condition_t* cond) {
    cond->waiting_thread = NULL;
    cond->waiting = false;
    cond->data = NULL;
}

void tl_condition_wait(condition_t* cond) {
    cond->waiting = true;
    cond->waiting_thread = current_thread;
    current_thread->state = STATE_BLOCKED;
    current_thread->syscall = SYSCALL_condition_wait;
    current_thread->data.condition.cond = cond;
    yield();
}

bool tl_condition_time_wait(condition_t* cond, struct etimer** timers, size_t numberofTimers) {
    cond->waiting = true;
    cond->waiting_thread = current_thread;
    current_thread->state = STATE_BLOCKED;
    current_thread->syscall = SYSCALL_condition_time_wait;
    current_thread->data.condition.cond = cond;
    current_thread->data.condition.timers = timers;
    current_thread->data.condition.numberofTimers = numberofTimers;
    yield();
    bool timeout = cond->waiting;
    cond->waiting = false;
    return timeout;
}

void tl_condition_signal(condition_t* cond, void* data) {
    if (! cond->waiting) return;

    cond->waiting = false;
    cond->data = data;
    process_post(PROCESS_CURRENT(), event_cond_signal, cond->waiting_thread);
}
