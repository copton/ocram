#include "core.h"
#include "dispatcher.h"
#include "logger.h"
#include "random.h"

#include <assert.h>
#include <string.h>

int handle;

#define FLASH_MAX 2
#define FILE_MAX 1024
typedef struct {
    uint8_t data[FILE_MAX];
    size_t offset;    
} File;

char* fileNames[FLASH_MAX];
File files[FLASH_MAX];
int fileHandle;

void os_init()
{
    handle = 0;
    fileHandle = 0;
    for (int i=0; i<FLASH_MAX; i++) {
        fileNames[i] = NULL;
    }

    logger_init();
    dispatcher_init();
    random_init();
}

void os_run()
{
    dispatcher_run();
}

uint32_t os_now()
{
    return dispatcher_now();
}

int os_listen(const char* address)
{
    return handle++;
}

int os_connect(const char* address)
{
    return handle++;
}

int os_flash_open(const char* address, Mode mode)
{
    for (int i=0; i<fileHandle; i++) {
        if (strcmp(address, fileNames[i]) == 0) {
            return i; 
        }
    }
    assert (fileHandle < FLASH_MAX);
    fileNames[fileHandle] = address;
    files[fileHandle].offset = 0;
    return fileHandle++;
}

error_t os_flash_seek(int handle, int offset)
{
    // TODO;
    return SUCCESS;
}

int os_sensor_open(const char* address)
{
    return handle++;
}

typedef struct {
    int seq;
    DefaultCallback cb;
    void* ctx;
    error_t error;
} CtxDefault;

static void cb_default(void* innerCtx)
{
    CtxDefault* ctx = innerCtx;
    logger_syscall_return(ctx->seq);
    ctx->cb(ctx->ctx, ctx->error); 
    free(ctx);
}

typedef struct {
    int seq;
    ReceiveCallback cb;
    void* ctx;
    error_t error;
    size_t len;
} CtxReceive;

static void cb_receive(void* innerCtx)
{
    CtxReceive* ctx = innerCtx;
    logger_syscall_return(ctx->seq);
    ctx->cb(ctx->ctx, ctx->error, ctx->len);
    free(ctx);
}

typedef struct {
    int seq;
    SensorReadCallback cb;
    void* ctx;
    error_t error;
    sensor_val_t value;
} CtxSensorRead;

static void cb_sensor_read(void* innerCtx)
{
    CtxSensorRead* ctx = innerCtx;
    logger_syscall_return(ctx->seq);
    ctx->cb(ctx->ctx, ctx->error, ctx->value);
    free(ctx);
}

void os_sleep(DefaultCallback cb, void* appCtx, uint32_t ms)
{
    CtxDefault* ctx = malloc(sizeof(CtxDefault));
    ctx->cb = cb;
    ctx->ctx = appCtx;
    ctx->error = random_error();
    ctx->seq = logger_syscall("sleep", "%u", ms);
    dispatcher_enqueue(ms, cb_default, ctx);
}

void os_receive(ReceiveCallback cb, void* appCtx, int handle, uint8_t* buffer, size_t buflen)
{
    CtxReceive* ctx = malloc(sizeof(CtxReceive));
    ctx->cb = cb;
    ctx->ctx = appCtx;
    ctx->error = random_error();
    ctx->len = random_integer_range(1, buflen);
    random_string((char*)buffer, ctx->len);
    uint32_t eta = random_integer_range(10, 1000);
    ctx->seq = logger_syscall("receive", "%d, %lu, %s", handle, (unsigned long)buflen, array(buffer, ctx->len));
    dispatcher_enqueue(eta, cb_receive, ctx);
}

void os_send(DefaultCallback cb, void* appCtx, int handle, uint8_t* buffer, size_t len)
{
    CtxDefault* ctx = malloc(sizeof(CtxDefault));
    ctx->cb = cb;
    ctx->ctx = appCtx;
    ctx->error = random_error();
    uint32_t eta = random_integer_range(1, 5);
    ctx->seq = logger_syscall("send", "%d, %s", handle, array(buffer, len));
    dispatcher_enqueue(eta, cb_default, ctx);
}

void os_flash_read(ReceiveCallback cb, void* appCtx, int handle, uint8_t* buffer, size_t len)
{
    assert (handle < fileHandle);
    assert (len >= files[handle].offset);

    CtxReceive* ctx = malloc(sizeof(CtxReceive));
    ctx->cb = cb;
    ctx->ctx = appCtx;
    ctx->error = SUCCESS;
    memcpy(buffer, files[handle].data, files[handle].offset);
    ctx->len = files[handle].offset;
    files[handle].offset = 0;
    uint32_t eta = random_integer_range(1, 10);
    ctx->seq = logger_syscall("flash_read", "%d, %lu, %s", handle, (unsigned long)len, array(buffer, ctx->len));
    dispatcher_enqueue(eta, cb_receive, ctx);
}

void os_flash_write(DefaultCallback cb, void* appCtx, int handle, uint8_t* buffer, size_t len)
{
    assert (handle < fileHandle);
    assert (files[handle].offset + len < FILE_MAX);

    CtxDefault* ctx = malloc(sizeof(CtxDefault));
    ctx->cb = cb;
    ctx->ctx = appCtx;
    ctx->error = random_error();
    if (ctx->error == SUCCESS) {
        memcpy(&files[handle].data[files[handle].offset], buffer, len);
        files[handle].offset += len;
    }
    uint32_t eta = random_integer_range(1, 7);
    ctx->seq = logger_syscall("flash_write", "%d, %s", handle, array(buffer, len));
    dispatcher_enqueue(eta, cb_default, ctx);
}

void os_sensor_read(SensorReadCallback cb, void* appCtx, int handle)
{
    CtxSensorRead* ctx = malloc(sizeof(CtxSensorRead));
    ctx->cb = cb;
    ctx->ctx = appCtx;
    ctx->error = random_error();
    ctx->value = random_integer(100);
    uint32_t eta = random_integer_range(1, 3);
    ctx->seq = logger_syscall("sensor_read", "%d, %u", handle, ctx->value);
    dispatcher_enqueue(eta, cb_sensor_read, ctx);
}
