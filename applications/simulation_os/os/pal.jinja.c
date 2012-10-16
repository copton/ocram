#include "os/common.h"
/* pal_code */
#include "os/core.h"
#include "os/logger.h"
#include <stdlib.h>

typedef void (*thread_t)(void*);
static int ctid = -1;
static thread_t* threads;

void pal_init(int numberOfThreads)
{
    threads = (thread_t*)malloc(numberOfThreads * sizeof(thread_t));
    os_init();
}

void pal_start_thread(thread_t thread)
{
    ++ctid;
    threads[ctid] = thread;
    thread(NULL);
}

void pal_run()
{
    os_run();
}

typedef struct {
    void* frame;
    int tid;
} Context;

#define NEW_CONTEXT \
    Context* ctx = malloc(sizeof(Context)); \
    ctx->frame = frame; \
    ctx->tid = ctid;

#define RETURN_FROM_SYSCALL \
    ctid = ctx->tid; \
    free(ctx); \
    frame->ec_result = result; \
    threads[ctid](frame->ec_cont);

/*{ if "tc_sleep" in all_syscalls }*/
static void sleep_cb(void* context, error_t result)
{
    Context* ctx = (Context*) context;
    ec_tframe_tc_sleep_t* frame = (ec_tframe_tc_sleep_t*) ctx->frame;
    RETURN_FROM_SYSCALL;
}

void tc_sleep(ec_tframe_tc_sleep_t * frame)
{
    NEW_CONTEXT;
	os_sleep(&sleep_cb, ctx, frame->ms);
}
/*{ endif }*/

/*{ if "tc_receive" in all_syscalls }*/
static void receive_cb(void* context, error_t result, size_t len)
{
    Context* ctx = (Context*) context;
	ec_tframe_tc_receive_t* frame = (ec_tframe_tc_receive_t*) ctx->frame;
	*(frame->len) = len;
    RETURN_FROM_SYSCALL;
}

void tc_receive(ec_tframe_tc_receive_t * frame)
{
    NEW_CONTEXT;
	os_receive(&receive_cb, ctx, frame->handle, frame->buffer, frame->buflen);
}
/*{ endif }*/

/*{ if "tc_send" in all_syscalls }*/
static void send_cb(void* context, error_t result)
{
    Context* ctx = (Context*) context;
	ec_tframe_tc_send_t* frame = (ec_tframe_tc_send_t*) ctx->frame;
    RETURN_FROM_SYSCALL;
}

void tc_send(ec_tframe_tc_send_t * frame)
{
    NEW_CONTEXT;
	os_send(&send_cb, ctx, frame->handle, frame->buffer, frame->len);
}
/*{ endif }*/

/*{ if "tc_flash_read" in all_syscalls }*/
static void flash_read_cb(void* context, error_t result, size_t len)
{
    Context* ctx = (Context*) context;
	ec_tframe_tc_flash_read_t* frame = (ec_tframe_tc_flash_read_t*) ctx->frame;
    *(frame->len) = len;
    RETURN_FROM_SYSCALL;
}

void tc_flash_read(ec_tframe_tc_flash_read_t* frame)
{
    NEW_CONTEXT;
    os_flash_read(&flash_read_cb, ctx, frame->handle, frame->buffer, frame->buflen);
}
/*{ endif }*/

/*{ if "tc_flash_write" in all_syscalls }*/
static void flash_write_cb(void* context, error_t result)
{
    Context* ctx = (Context*) context;
	ec_tframe_tc_flash_write_t* frame = (ec_tframe_tc_flash_write_t*) ctx->frame;
    RETURN_FROM_SYSCALL;
}

void tc_flash_write(ec_tframe_tc_flash_write_t * frame)
{
    NEW_CONTEXT;
	os_flash_write(&flash_write_cb, ctx, frame->handle, frame->buffer, frame->len);
}
/*{ endif }*/

/*{ if "tc_sensor_read" in all_syscalls }*/
static void sensor_read_cb(void* context, error_t result, sensor_val_t value)
{
    Context* ctx = (Context*) context;
	ec_tframe_tc_sensor_read_t* frame = (ec_tframe_tc_sensor_read_t*) ctx->frame;
	*(frame->value) = value;
    RETURN_FROM_SYSCALL;
}

void tc_sensor_read(ec_tframe_tc_sensor_read_t * frame)
{
    NEW_CONTEXT;
	os_sensor_read(&sensor_read_cb, ctx, frame->handle);
}
/*{ endif }*/

/*{ for _ in thread_syscalls }*/
void ec_thread_/*loop.index0*/(void*);
/*{ endfor }*/

int main()
{
    pal_init(/*numberof_threads*/);
/*{ for _ in thread_syscalls }*/
    pal_start_thread(&ec_thread_/*loop.index0*/);
/*{ endfor }*/
    pal_run();
    return 0;
}
