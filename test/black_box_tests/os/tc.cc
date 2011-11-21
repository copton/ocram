#include "tc.h"
#include "ec.h"
#include <pthread.h>

class Monitor {
public:
    Monitor()
    {
        pthread_mutex_init(&mutex, NULL);
    }

    void enter()
    {
        pthread_mutex_lock(&mutex);
    }

    void leave()
    {
        pthread_mutex_unlock(&mutex);
    }

private:
    friend class Condition;
    pthread_mutex_t mutex;
} monitor;

class Condition {
public:
    Condition()
        : waiting(false)
    {
        pthread_cond_init(&condition, NULL);
    }

    void wait()
    {
        waiting = true;
        while (waiting) {
            pthread_cond_wait(&condition, &monitor.mutex);
        }
    }

    void signal()
    {
        waiting = false;
        pthread_cond_signal(&condition);
    }

private:
    pthread_cond_t condition;
    bool waiting;
} sync;

struct DefaultContext {
    Condition condition;
    error_t result;  
    bool flag;
};

struct WriteContext : public DefaultContext {
};

struct ReadContext : public DefaultContext {
    uint8_t len; 
};

void defaultCallback(void* ctx, error_t result)
{
    DefaultContext* context = (DefaultContext*) ctx;
    context->result = result;
    context->condition.signal();
    sync.wait();
}

void writeCallback(void* ctx, error_t result)
{
    defaultCallback(ctx, result);
}

void readCallback(void* ctx, error_t result, uint8_t len)
{
    ((ReadContext*) ctx)->len = len;
    defaultCallback(ctx, result);
}

error_t tc_sleep(uint32_t ms)
{
    DefaultContext ctx;
    ec_sleep(&defaultCallback, &ctx, ms);  
    sync.signal();
    ctx.condition.wait();
    return ctx.result;
}

error_t tc_receive(void* handle, unsigned char* buffer, uint8_t buflen, uint8_t* len)
{
    ReadContext ctx;
    ec_receive(&readCallback, &ctx, handle, buffer, buflen);
    sync.signal();
    ctx.condition.wait();
    *len = ctx.len;
    return ctx.result;
}

error_t tc_send(void* handle, unsigned char* buffer, uint8_t len)
{
    WriteContext ctx;
    ec_send(&writeCallback, &ctx, handle, buffer, len);
    sync.signal();
    ctx.condition.wait();
    return ctx.result;
}

error_t tc_flash_read(void* handle, unsigned char* buffer, uint8_t buflen, uint8_t* len)
{
    ReadContext ctx;
    ec_flash_read(&readCallback, &ctx, handle, buffer, buflen);
    sync.signal();
    ctx.condition.wait();
    *len = ctx.len;
    return ctx.result;
}

error_t tc_flash_write(void* handle, unsigned char* buffer, uint8_t len)
{
    WriteContext ctx;
    ec_flash_write(&writeCallback, &ctx, handle, buffer, len);
    sync.signal();
    ctx.condition.wait();
    return ctx.result;
}

error_t tc_sensor_read(void* handle, unsigned char* buffer, uint8_t buflen, uint8_t* len)
{
    ReadContext ctx;
    ec_sensor_read(&readCallback, &ctx, handle, buffer, buflen);
    sync.signal();
    ctx.condition.wait();
    *len = ctx.len;
    return ctx.result;
}

