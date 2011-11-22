#include "tc.h"
#include "ec.h"
#include <pthread.h>
#include <assert.h>
#include <vector>
#include <iostream>

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

std::vector<pthread_t*> threads;

void tc_join_thread(int handle)
{
    assert (handle >= 0);
    assert (handle < threads.size());
    if (threads[handle] == 0) {
        std::cerr << "warning: thread " << handle << " has already been joined." << std::endl;
    } else {
        pthread_join(*threads[handle], 0);
        delete threads[handle];
        threads[handle] = 0;
    }
}

void* run_thread(void* ctx)
{
    void(*thread_start_function)() = (void(*)()) ctx;
    mutex.enter();
    thread_start_function();
    mutex.leave();
    return 0;
}

int tc_run_thread(void(*thread_start_function)())
{
    pthread_t* thread = new pthread_t();
    threads.push_back(thread);
    pthread_create(thread, 0, run_thread, (void*)thread_start_function);
    return threads.size() - 2;
}


struct DefaultContext {
    Condition condition;
    error_t result;  
};

void defaultCallback(void* ctx, error_t result)
{
    DefaultContext* context = (DefaultContext*) ctx;
    context->result = result;
    context->condition.signal();
    condition.wait();
}

error_t tc_sleep(uint32_t ms)
{
    DefaultContext ctx;
    ec_sleep(&defaultCallback, &ctx, ms);  
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

struct ReceiveContext : public DefaultContext {
    size_t len; 
};

void receiveCallback(void* ctx, error_t result, size_t len)
{
    ((ReceiveContext*) ctx)->len = len;
    defaultCallback(ctx, result);
}

error_t tc_receive(int handle, uint8_t* buffer, size_t buflen, size_t* len)
{
    ReceiveContext ctx;
    ec_receive(&receiveCallback, &ctx, handle, buffer, buflen);
    condition.signal();
    ctx.condition.wait();
    *len = ctx.len;
    return ctx.result;
}

error_t tc_send(int handle, uint8_t* buffer, size_t len)
{
    DefaultContext ctx;
    ec_send(&defaultCallback, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

error_t tc_flash_read(int handle, uint8_t* buffer, size_t buflen, size_t* len)
{
    ReceiveContext ctx;
    ec_flash_read(&receiveCallback, &ctx, handle, buffer, buflen);
    condition.signal();
    ctx.condition.wait();
    *len = ctx.len;
    return ctx.result;
}

error_t tc_flash_write(int handle, uint8_t* buffer, size_t len)
{
    DefaultContext ctx;
    ec_flash_write(&defaultCallback, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

struct SensorReadContext : public DefaultContext {
    sensor_val_t value;
};

void callbackSensorRead(void* ctx, error_t result, sensor_val_t value)
{
    ((SensorReadContext*)ctx)->value = value;
    defaultCallback(ctx, result);
}

error_t tc_sensor_read(int handle, sensor_val_t* value)
{
    SensorReadContext ctx;
    ec_sensor_read(&callbackSensorRead, &ctx, handle);
    condition.signal();
    ctx.condition.wait();
    return ctx.result;
}

