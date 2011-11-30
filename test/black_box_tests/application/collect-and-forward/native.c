#include "os/core.h"

void sensorReadDone(void* ctx, error_t result, sensor_val_t value);
void sleepDone(void* ctx, error_t result);
void receiveDone(void* ctx, error_t result, size_t len);
void sendDone(void* ctx, error_t result);
void flashReadDone(void* ctx, error_t result, size_t len);
void flashWriteDone(void* ctx, error_t result);

enum Task {
    COLLECT,
    RECEIVE,
    SEND
};

typedef struct {
    Task task;
    void* taskContext;
} Context;

typedef struct {
    int sensor;
    int log;
    unsigned dt;
    sensor_val_t value;
} CollectContext;

typedef struct {
    int socket;
    int log;
    uint8_t buffer[10];
} ReceiveContext;

typedef struct {
    int log1;
    int log2;
    int socket;
    unsigned dt;
    int min;
    int max;
    int state;
    uint8_t buffer[1024];
    uint8_t payload[2 * sizeof(int32_t)];
}

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
                os_flash_write(&flashWriteDone, ctx, taskCtx->log, &taskCtx->value, sizeof(sensor_val_t));
            }
        } break;
    }
}

void sleepDone(void* ctx, error_t result)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case COLLECT: {
            CollectContext* taskCtx = (CollectContext*) genericCtx->taskCtx;
            os_sensor_read(&sensorReadDone, ctx, taskCtx->sensorDevice); 
        } break;
        case SEND: {
            SendContext* taskCtx = (SendContext*) genericCtx->taskCtx;
            taskCtx->min = 0x7FFFFFFF;
            taskCtx->max = 0xFFFFFFFF;
            taskCtx->state = 1;
            os_flash_read(&flashReadDone, ctx, taskCtx->log1); 
        } break;
    }
}

void receiveDone(void* ctx, error_t result, size_t len)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case RECEIVE: {
            ReceiveContext* taskCtx = (ReceiveContext*) genericCtx->taskCtx;
            if (result != SUCCESS) {
                os_receive(&receiveDone, ctx, taskCtx->socket, &taskCtx->buffer, sizeof(taskCtx->buffer));
            } else {
                len -= len % sizeof(int32_t);
                os_flash_write(&flashWriteDone, ctx, taskCtx->log, taskCtx->buffer, len);
            }
          } break;
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
                os_sleep(&sleepDone, ctx, taskCtx->dt);
            }
       } break;
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
                if (tmp < *taskCtx->min) *taskCtx->min = tmp;
                if (tmp > *taskCtx->max) *taskCtx->max = tmp;
            }
            if (taskCtx->state == 1) {
                taskCtx->state = 2;
                os_flash_read(&flashReadDone, ctx, taskCtx->log2); 
            } else {
                memcpy(taskCtx->payload, &taskCtx->min, sizeof(int32_t));
                memcpy(taskCtx->payload + sizeof(int32_t), &taskCtx->max, sizeof(int32_t));
                os_send(&sendDone, ctx, taskCtx->socket, taskCtx->payload, sizeof(taskCtx->payload));
            }
        } break;
    }
}

void flashWriteDone(void* ctx, error_t result)
{
    Context* genericCtx = (Context*) ctx;
    switch (genericCtx->task) {
        case COLLECT: {
            CollectContext* taskCtx = (CollectContext*) genericCtx->taskCtx;
            if (result != SUCCESS) {
                os_flash_write(&flashWriteDone, ctx, taskCtx->log, &taskCtx->value, sizeof(sensor_val_t));
            } else {
                os_sleep(&sleepDone, ctx, taskCtx->dt);
            }
        } break;
        case RECEIVE: {
            ReceiveContext* taskCtx = (ReceiveContext*) genericCtx->taskCtx;
            if (result != SUCCESS) {
                os_flash_write(&flashWriteDone, ctx, taskCtx->log, taskCtx->buffer, len);
            } else {
                os_receive(&receiveDone, ctx, taskCtx->socket, &taskCtx->buffer, sizeof(taskCtx->buffer));
            }
        } break;
    }
}

void collectInit(const char* device, const char* file, unsigned dt)
{
    Context* genericCtx = malloc(sizeof(Context));
    CollectContext* taskCtx = malloc(sizeof(CollectContext));
    genericCtx->task = COLLECT;
    genericCtx->taskContext = taskCtx;

    taskCtx->dt = dt;
    taskCtx->sensor = os_sensor_open(device);
    taskCtx->log = os_flash_open(file, WRITE);

    os_sleep(&sleepDone, genericCtx, taskCtx->dt);
}

void receiveInit(const char* channel, const char* file)
{
    Context* genericCtx = malloc(sizeof(Context));
    ReceiveContext* taskCtx = malloc(sizeof(ReceiveContext));
    genericCtx->task = RECEIVE;
    genericCtx->taskContext = taskCtx;

    taskCtx->socket = os_listen(channel);
    taskCtx->log = os_flash_open(file, WRITE);
    os_receive(&receiveDone, genericCtx, taskCtx->socket, &taskCtx->buffer, sizeof(taskCtx->buffer));
}

void sendInit(const char* channel, const char* file1, const char* file2, unsigned dt)
{
    Context* genericCtx = malloc(sizeof(Context));
    SendContext* taskCtx = malloc(sizeof(SendContext));
    genericCtx->task = SEND;
    genericCtx->taskContext = taskCtx;
    
    taskCtx->log1 = os_flash_open(file1, READ);
    taskCtx->log2 = os_flash_open(file2, READ);
    taskCtx->socket = os_connect(channel);
    taskCtx->dt = dt;
    os_sleep(&sleepDone, genericCtx, taskCtx->dt);
}

int main()
{
    os_init();

    collectInit("sensor", "sensor_log", 50);
    sendInit("parent", "receive_log", "sensor_log", 100);
    receiveInit("child", "receive_log");

    os_run();

    return 0;
}
