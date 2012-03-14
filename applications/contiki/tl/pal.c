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
 
#include "sim_assert.h"
#include <stdint.h>
#include "contiki.h"
#include "clock.h"

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
 
#define PREPARE_STACK()                               \
  SWAP_STACK_PTR(old_stack_ptr, current_thread->sp);  \
  __asm__("push %0":: "r" (thread_wrapper));          \
  __asm__("eint");                                    \
  __asm__("push r2");                                 \
  __asm__("dint");                                    \
  {int i; for(i=0;i<12;i++) {__asm__("push r3");}}    \
  SWAP_STACK_PTR(current_thread->sp, old_stack_ptr)

//This must be a power of 2
#define TOSH_MAX_THREADS      4
#define TOSH_MAX_THREADS_MASK (TOSH_MAX_THREADS - 1)
enum {
  STATE_NULL = 0,     //There is no thread here
  STATE_READY = 1,    //This thread has been created but never executed before
  STATE_ACTIVE = 2,   //There is a thread here, not blocked or sleeping
  STATE_BLOCKED = 3,  //This thread is blocked on a mutex
  STATE_SLEEP = 4,    //This thread is sleep and can be woken up
  STATE_IO = 5        //This thread is blocked until I/O Completion
};

typedef enum {
    SYSCALL_tc_receive,
    SYSCALL_tc_sleep,
    SYSCALL_tc_condition_wait,
    SYSCALL_tc_condition_time_wait,
} Syscall;

typedef struct {
    clock_time_t tics;
} ctx_sleep_t;

typedef struct thread{
  volatile uint16_t * sp;
  volatile uint8_t state;
  volatile union{
    void (*tp) ();
    //mutex * m;
    uint16_t sleep;
    struct {
      uint8_t type, id;
    }io;
  }data;
  volatile union {
    ctx_sleep_t sleep; 
  } ctx;
  volatile Syscall syscall;
  struct process* process;
}thread;

void __attribute__((naked))  thread_wrapper();
uint8_t interested = 0;

thread thread_table[TOSH_MAX_THREADS];

// A pointer to the current thread; is null when in regular tinyos thread
volatile thread * current_thread;

// The index of the current thread
volatile uint16_t thread_idx = -1;

// The tinyos stack pointer while we are executing a thread
volatile uint16_t * old_stack_ptr = 0;

// Keep track of whether the TinyThread scheduler task is running
volatile uint8_t process_scheduler_active = 0;

PROCESS(process_scheduler, "scheduler");
PROCESS(process_thread_0, "thread_0");
PROCESS(process_thread_1, "thread_1");

__attribute__((noinline)) static void service_threads() {
    uint8_t i;

    if (process_scheduler_active)
        return;

    for (i=0; i<TOSH_MAX_THREADS; i++) {
        if (thread_table[i].state == STATE_ACTIVE) {
            break;
        }
    }

    process_scheduler_active = (i != TOSH_MAX_THREADS);
    if (process_scheduler_active) {
        process_poll(&process_scheduler);
    }
}

static void init() {
    memset(thread_table, 0, sizeof(thread) * TOSH_MAX_THREADS);
    thread_idx = 0;
    current_thread = 0;
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
    yield() ;
}

static uint8_t is_thread() {
    return (!!current_thread);
}

static void call_fcn_ptr(void (*tp)()){
  (*tp)();
}

__attribute__((naked)) void thread_wrapper() {
    call_fcn_ptr(current_thread->data.tp);
    remove_thread();
}

static int create_thread(int tid, void (*fcn)(), uint16_t * stack_ptr) {
    if (is_thread())
        return(0);

    ASSERT(thread_table[tid].state == STATE_NULL);

    current_thread = &thread_table[tid];
    current_thread->process = PROCESS_CURRENT();
    current_thread->sp = stack_ptr;
    current_thread->state = STATE_ACTIVE;
    current_thread->data.tp = fcn;

    PREPARE_STACK();
    current_thread = 0;
    
    if (!process_scheduler_active) {
        process_poll(&process_scheduler);
        process_scheduler_active = 1;
    }
    return (1);
}

static void wakeup_thread(uint8_t id) {
    if (id >= TOSH_MAX_THREADS) {
        return;
    }

    if (thread_table[id].state == STATE_SLEEP) {
        thread_table[id].state = STATE_ACTIVE;
    }

    service_threads();
}

PROCESS_THREAD(process_scheduler, ev, data)
{
    PROCESS_BEGIN();
    init();
    process_start(&process_thread_0, NULL);
    process_start(&process_thread_1, NULL);
    while(1) {
        PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);

        int i;
        for (i = 0; i<TOSH_MAX_THREADS; i++) {
            uint8_t j = TOSH_MAX_THREADS_MASK & (thread_idx + i);
            if (thread_table[j].state == STATE_ACTIVE)
                break;
        }

        if (i != TOSH_MAX_THREADS) {
            i = TOSH_MAX_THREADS_MASK & (thread_idx + i);
            thread_idx = i;
            current_thread = &(thread_table[i]);
            if (thread_table[i].state == STATE_ACTIVE) {
                platform_switch_to_thread();
            }
            thread_idx++;
            thread_idx = (TOSH_MAX_THREADS_MASK & (thread_idx));
        }
        current_thread = 0;
        process_scheduler_active = 0;
        service_threads();
    }
    PROCESS_END();
}

AUTOSTART_PROCESSES(&process_scheduler);

uint16_t* stack_top(uint8_t* stack_, size_t size){
    uint16_t* stack = (uint16_t*) stack_;
    return(&(stack[(size / sizeof(uint16_t)) - 1]));
}

void app_thread_0();
extern uint8_t app_stack_0[];
extern size_t app_stack_size_0;

PROCESS_THREAD(process_thread_0, ev, data)
{
    PROCESS_BEGIN();
    ASSERT((app_stack_size_0 % 2) == 0);
    create_thread(0, &app_thread_0, stack_top(app_stack_0, app_stack_size_0));
    while(1) {
        PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);
        if (0) { } 
        else if (thread_table[0].syscall == SYSCALL_tc_sleep) {
            static struct etimer et;
            clock_time_t now = clock_time();
            if (thread_table[0].ctx.sleep.tics > now) {
                etimer_set(&et, thread_table[0].ctx.sleep.tics - now);
                PROCESS_YIELD_UNTIL(etimer_expired(&et));
            } else {
                PROCESS_PAUSE();
            }
            wakeup_thread(0);
        } else {
           ASSERT(0); 
        }
    }

    PROCESS_END();
}

void app_thread_1();
extern uint8_t app_stack_1[];
extern size_t app_stack_size_1;

PROCESS_THREAD(process_thread_1, ev, data)
{
    PROCESS_BEGIN();
    ASSERT((app_stack_size_0 % 2) == 0);
    create_thread(1, &app_thread_1, stack_top(app_stack_1, app_stack_size_1));
    while(1) {
        PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);
        if (0) { } 
        else if (thread_table[1].syscall == SYSCALL_tc_sleep) {
            static struct etimer et;
            clock_time_t now = clock_time();
            if (thread_table[1].ctx.sleep.tics > now) {
                etimer_set(&et, thread_table[1].ctx.sleep.tics - now);
                PROCESS_YIELD_UNTIL(etimer_expired(&et));
            } else {
                PROCESS_PAUSE();
            }
            wakeup_thread(1);
        } else {
           ASSERT(0); 
        }
    }

    PROCESS_END();
}

void tl_sleep(clock_time_t tics) {
    if (!is_thread())
        return;

    current_thread->state = STATE_SLEEP;
    current_thread->syscall = SYSCALL_tc_sleep;
    current_thread->ctx.sleep.tics = tics;
    process_poll(current_thread->process);
    yield();
}
