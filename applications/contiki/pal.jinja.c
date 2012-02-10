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
/*{ if "tc_coap_send_transaction" in all_syscalls }*/
#include "process.h"
#include "er-coap-07.h"
#include "er-coap-07-transactions.h"
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
/*{ if syscall == "tc_coap_send_transaction" }*/
typedef struct {
    ec_frame_tc_coap_send_transaction_t* frame;
    coap_packet_t* response;
    struct process* process;
} ec_ctx_tc_coap_send_transaction_t;
/*{ else }*/
typedef struct {
    ec_frame_/*syscall*/_t* frame;
} ec_ctx_/*syscall*/_t;
/*{ endif }*/
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
void tc_sleep(ec_frame_tc_sleep_t* frame)
{
    threads[tid].ctx.tc_sleep.frame = frame;
    threads[tid].syscall = SYSCALL_tc_sleep;
}
/*{ endif }*/

/*{ if "tc_receive" in all_syscalls }*/
void tc_receive(ec_frame_tc_receive_t* frame)
{
    threads[tid].ctx.tc_receive.frame = frame;
    threads[tid].syscall = SYSCALL_tc_receive;
}
/*{ endif }*/

/*{ if "tc_send" in all_syscalls }*/
void tc_send(ec_frame_tc_send_t* frame)
{
    threads[tid].ctx.tc_send.frame = frame;
    threads[tid].syscall = SYSCALL_tc_send;
}
/*{ endif }*/

/*{ if "tc_await_button" in all_syscalls }*/
void tc_await_button(ec_frame_tc_await_button_t* frame)
{
    threads[tid].ctx.tc_await_button.frame = frame;
    threads[tid].syscall = SYSCALL_tc_await_button;
}
/*{ endif }*/

/*{ if "tc_coap_send_transaction" in all_syscalls }*/
void tc_coap_send_transaction(ec_frame_tc_coap_send_transaction_t* frame)
{
    threads[tid].ctx.tc_coap_send_transaction.frame = frame;
    threads[tid].syscall = SYSCALL_tc_coap_send_transaction;
}

void coap_transaction_callback(void* data, void* response)
{
    ec_ctx_tc_coap_send_transaction_t* ctx = data;
    ctx->response = response;
    process_poll(ctx->process);
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
                PROCESS_WAIT_UNTIL(etimer_expired(&et));
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
            frame = threads[/*loop.index0*/].ctx.tc_receive.frame;
            if (frame->buflen < uip_datalen()) {
                *frame->len = frame->buflen;
            } else {
                *frame->len = uip_datalen();
            }
            memcpy(frame->buffer, uip_appdata, *frame->len);
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
/*{ if "tc_coap_send_transaction" in all_syscalls }*/
        else if (threads[/*loop.index0*/].syscall == SYSCALL_tc_coap_send_transaction) {
            ec_ctx_tc_coap_send_transaction_t* ctx = &threads[/*loop.index0*/].ctx.tc_coap_send_transaction;
            ctx->process = PROCESS_CURRENT();
            ctx->frame->transaction->callback = &coap_transaction_callback;
            ctx->frame->transaction->callback_data = ctx;
            coap_send_transaction(ctx->frame->transaction);
            PROCESS_YIELD_UNTIL(ev == PROCESS_EVENT_POLL);
            ctx = &threads[/*loop.index0*/].ctx.tc_coap_send_transaction;
            ctx->frame->ec_result = ctx->response;
            continuation = ctx->frame->ec_cont;
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

