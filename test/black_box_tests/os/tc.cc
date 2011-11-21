#include "tc.h"
#include "ec.h"
#include <pthread.h>

class Mutex {
public:
    Mutex()
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
} mutex;

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
            pthread_cond_wait(&condition, &mutex.mutex);
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
} condition;

struct Context2 {
    Condition condition;
    error_t result;  
    bool flag;
};

struct Context3 : public Context2 {
    size_t len; 
};

void callback2(void* ctx, error_t result)
{
    Context2* context = (Context2*) ctx;
    context->result = result;
    context->condition.signal();
    condition.wait();
}

void callback3(void* ctx, error_t result, size_t len)
{
    ((Context3*) ctx)->len = len;
    callback2(ctx, result);
}

error_t tc_sleep(uint32_t ms)
{
    Context2 ctx;
    ec_sleep(&callback2, &ctx, ms);  
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

error_t tc_receive(int handle, uint8_t* buffer, size_t buflen, size_t* len)
{
    Context3 ctx;
    ec_receive(&callback3, &ctx, handle, buffer, buflen);
    condition.signal();
    ctx.condition.wait();
    *len = ctx.len;
    return ctx.result;
}

error_t tc_send(int handle, uint8_t* buffer, size_t len)
{
    Context2 ctx;
    ec_send(&callback2, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

error_t tc_flash_read(int handle, uint8_t* buffer, size_t len)
{
    Context2 ctx;
    ec_flash_read(&callback2, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

error_t tc_flash_write(int handle, uint8_t* buffer, size_t len)
{
    Context2 ctx;
    ec_flash_write(&callback2, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

error_t tc_sensor_read(int handle, uint8_t* buffer, size_t len)
{
    Context2 ctx;
    ec_sensor_read(&callback2, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

