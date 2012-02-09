#include "os/core.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

void sensorReadDone(void* ctx, error_t result, sensor_val_t value);
void sleepDone(void* ctx, error_t result);
void receiveDone(void* ctx, error_t result, size_t len);
void sendDone(void* ctx, error_t result);
void flashReadDone(void* ctx, error_t result, size_t len);
void flashWriteDone(void* ctx, error_t result);

typedef enum {
    COLLECT,
    RECEIVE,
    SEND
} Task;

typedef struct {
    Task task;
    void* taskCtx;
} Context;

typedef struct {
    int sensor;
    int log;
    unsigned dt;
    uint32_t now;
    sensor_val_t value;
} CollectContext;

typedef struct {
    int socket;
    int log;
    uint8_t buffer[10];
    size_t len;
} ReceiveContext;

typedef struct {
    int log1;
    int log2;
    int socket;
    unsigned dt;
    uint32_t now;
    int32_t min;
    int32_t max;
    int state;
    uint8_t buffer[1024];
    uint8_t payload[2 * sizeof(int32_t)];
} SendContext;

CollectContext taskCollectContext;
ReceiveContext taskReceiveContext;
SendContext taskSendContext;

Context collectContext = { COLLECT, &taskCollectContext};
Context receiveContext = { RECEIVE, &taskReceiveContext};
Context sendContext = { SEND, &taskSendContext};

void sensorReadDone(void* ctx, error_t result, sensor_val_t value)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case COLLECT: {
            CollectContext* taskCtx = (CollectContext*) genericCtx->taskCtx;
            if (result != SUCCESS) {
                os_sensor_read(&sensorReadDone, ctx, taskCtx->sensor); 
            } else {
                taskCtx->value = value;
                os_flash_write(&flashWriteDone, ctx, taskCtx->log, (uint8_t*)&taskCtx->value, sizeof(sensor_val_t));
            }
        } break;
        default: assert(0);
    }
}

void sleepDone(void* ctx, error_t result)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case COLLECT: {
            CollectContext* taskCtx = (CollectContext*) genericCtx->taskCtx;
            taskCtx->now += taskCtx->dt;
            os_sensor_read(&sensorReadDone, ctx, taskCtx->sensor); 
        } break;
        case SEND: {
            SendContext* taskCtx = (SendContext*) genericCtx->taskCtx;
            taskCtx->now += taskCtx->dt;
            taskCtx->min = 0x7FFFFFFF;
            taskCtx->max = 0xFFFFFFFF;
            taskCtx->state = 1;
            os_flash_read(&flashReadDone, ctx, taskCtx->log1, taskCtx->buffer, sizeof(taskCtx->buffer)); 
        } break;
        default: assert(0);
    }
}

void receiveDone(void* ctx, error_t result, size_t len)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case RECEIVE: {
            ReceiveContext* taskCtx = (ReceiveContext*) genericCtx->taskCtx;
            if (result != SUCCESS) {
                os_receive(&receiveDone, ctx, taskCtx->socket, taskCtx->buffer, sizeof(taskCtx->buffer));
            } else {
                taskCtx->len = len - (len % sizeof(int32_t));
                os_flash_write(&flashWriteDone, ctx, taskCtx->log, taskCtx->buffer, taskCtx->len);
            }
          } break;
        default: assert(0);
    }
}

void sendDone(void* ctx, error_t result)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case SEND: {
            SendContext* taskCtx = (SendContext*) genericCtx->taskCtx;
            if (result != SUCCESS) {
                os_send(&sendDone, ctx, taskCtx->socket, taskCtx->payload, sizeof(taskCtx->payload));
            } else {
                os_sleep(&sleepDone, ctx, taskCtx->now + taskCtx->dt);
            }
       } break;
        default: assert(0);
    }
}

void flashReadDone(void* ctx, error_t result, size_t len)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case SEND: {
            SendContext* taskCtx = (SendContext*) genericCtx->taskCtx;
            assert(result == SUCCESS);
            assert((len % sizeof(int32_t)) == 0);
            assert(len < sizeof(taskCtx->buffer));
            int i;
            for (i=0; i < len / sizeof(int32_t); i++) {
                int32_t tmp;
                memcpy(&tmp, taskCtx->buffer + i * sizeof(int32_t), sizeof(int32_t));
                if (tmp < taskCtx->min) taskCtx->min = tmp;
                if (tmp > taskCtx->max) taskCtx->max = tmp;
            }
            if (taskCtx->state == 1) {
                taskCtx->state = 2;
                os_flash_read(&flashReadDone, ctx, taskCtx->log2, taskCtx->buffer, sizeof(taskCtx->buffer)); 
            } else {
                memcpy(taskCtx->payload, &taskCtx->min, sizeof(int32_t));
                memcpy(taskCtx->payload + sizeof(int32_t), &taskCtx->max, sizeof(int32_t));
                os_send(&sendDone, ctx, taskCtx->socket, taskCtx->payload, sizeof(taskCtx->payload));
            }
        } break;
        default: assert(0);
    }
}

void flashWriteDone(void* ctx, error_t result)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case COLLECT: {
            CollectContext* taskCtx = (CollectContext*) genericCtx->taskCtx;
            if (result != SUCCESS) {
                os_flash_write(&flashWriteDone, ctx, taskCtx->log, (uint8_t*)&taskCtx->value, sizeof(sensor_val_t));
            } else {
                os_sleep(&sleepDone, ctx, taskCtx->now + taskCtx->dt);
            }
        } break;
        case RECEIVE: {
            ReceiveContext* taskCtx = (ReceiveContext*) genericCtx->taskCtx;
            if (result != SUCCESS) {
                os_flash_write(&flashWriteDone, ctx, taskCtx->log, taskCtx->buffer, taskCtx->len);
            } else {
                os_receive(&receiveDone, ctx, taskCtx->socket, taskCtx->buffer, sizeof(taskCtx->buffer));
            }
        } break;
        default: assert(0);
    }
}

void collectInit(const char* device, const char* file, unsigned dt)
{
    taskCollectContext.dt = dt;
    taskCollectContext.sensor = os_sensor_open(device);
    taskCollectContext.log = os_flash_open(file, WRITE);
    taskCollectContext.now = os_now();

    os_sleep(&sleepDone, &collectContext, taskCollectContext.now + taskCollectContext.dt);
}

void receiveInit(const char* channel, const char* file)
{
    taskReceiveContext.socket = os_listen(channel);
    taskReceiveContext.log = os_flash_open(file, WRITE);
    os_receive(&receiveDone, &receiveContext, taskReceiveContext.socket, taskReceiveContext.buffer, sizeof(taskReceiveContext.buffer));
}

void sendInit(const char* channel, const char* file1, const char* file2, unsigned dt)
{
    taskSendContext.log1 = os_flash_open(file1, READ);
    taskSendContext.log2 = os_flash_open(file2, READ);
    taskSendContext.socket = os_connect(channel);
    taskSendContext.dt = dt;
    taskSendContext.now = os_now();
    os_sleep(&sleepDone, &sendContext, taskSendContext.now + taskSendContext.dt);
}

int main()
{
    os_init();

    collectInit("sensor", "sensor_log", 50);
    receiveInit("child", "receive_log");
    sendInit("parent", "receive_log", "sensor_log", 100);

    os_run();

    return 0;
}
