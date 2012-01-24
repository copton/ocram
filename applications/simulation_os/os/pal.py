#!/usr/bin/env python

import sys

if len(sys.argv) <= 1:
    sys.stderr.write("There must be at least one thread.\n")
    sys.exit(1)


def headers(out):
    out('#include "os/common.h"\n')

def static_part(out):
    out(r"""
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
    const int tid;
} Context;

#define NEW_CONTEXT \
    Context* ctx = malloc(sizeof(Context)); \
    ctx->frame = frame;

#define RETURN_FROM_SYSCALL \
    ctid = ctx->tid; \
    free(ctx); \
    frame->ec_result = result; \
    threads[ctid](frame->ec_cont);

static void sleep_cb(void* context, error_t result)
{
    Context* ctx = (Context*) context;
    ec_frame_tc_sleep_t* frame = (ec_frame_tc_sleep_t*) ctx->frame;
    RETURN_FROM_SYSCALL;
}

void tc_sleep(ec_frame_tc_sleep_t * frame)
{
    NEW_CONTEXT;
	os_sleep(&sleep_cb, ctx, frame->ms);
}

static void receive_cb(void* context, error_t result, size_t len)
{
    Context* ctx = (Context*) context;
	ec_frame_tc_receive_t* frame = (ec_frame_tc_receive_t*) ctx->frame;
	*(frame->len) = len;
    RETURN_FROM_SYSCALL;
}

void tc_receive(ec_frame_tc_receive_t * frame)
{
    NEW_CONTEXT;
	os_receive(&receive_cb, ctx, frame->handle, frame->buffer, frame->buflen);
}

static void send_cb(void* context, error_t result)
{
    Context* ctx = (Context*) context;
	ec_frame_tc_send_t* frame = (ec_frame_tc_send_t*) ctx->frame;
    RETURN_FROM_SYSCALL;
}

void tc_send(ec_frame_tc_send_t * frame)
{
    NEW_CONTEXT;
	os_send(&send_cb, ctx, frame->handle, frame->buffer, frame->len);
}

static void flash_read_cb(void* context, error_t result, size_t len)
{
    Context* ctx = (Context*) context;
	ec_frame_tc_flash_read_t* frame = (ec_frame_tc_flash_read_t*) ctx->frame;
    *(frame->len) = len;
    RETURN_FROM_SYSCALL;
}

void tc_flash_read(ec_frame_tc_flash_read_t* frame)
{
    NEW_CONTEXT;
    os_flash_read(&flash_read_cb, ctx, frame->handle, frame->buffer, frame->buflen);
}

static void flash_write_cb(void* context, error_t result)
{
    Context* ctx = (Context*) context;
	ec_frame_tc_flash_write_t* frame = (ec_frame_tc_flash_write_t*) ctx->frame;
    RETURN_FROM_SYSCALL;
}


void tc_flash_write(ec_frame_tc_flash_write_t * frame)
{
    NEW_CONTEXT;
	os_flash_write(&flash_write_cb, ctx, frame->handle, frame->buffer, frame->len);
}

static void sensor_read_cb(void* context, error_t result, sensor_val_t value)
{
    Context* ctx = (Context*) context;
	ec_frame_tc_sensor_read_t* frame = (ec_frame_tc_sensor_read_t*) ctx->frame;
	*(frame->value) = value;
    RETURN_FROM_SYSCALL;
}

void tc_sensor_read(ec_frame_tc_sensor_read_t * frame)
{
    NEW_CONTEXT;
	os_sensor_read(&sensor_read_cb, ctx, frame->handle);
}
""")

def dynamic_part(out, numberof_threads):
    out("\n".join(["void ec_thread_%d(void*);" % (tid+1) for tid in range(numberof_threads)]))
    out("""
int main()
{
    pal_init(%d);
""" % numberof_threads)
    out("\n".join(["pal_start_thread(&ec_thread_%d);" % (tid+1) for tid in range(numberof_threads)]))
    out("""
    pal_run();
    return 0;
}""")

numberof_threads = len(sys.argv) - 1
out = sys.stdout.write
headers(out)
out(sys.stdin.read())
static_part(out)
dynamic_part(out, numberof_threads)
