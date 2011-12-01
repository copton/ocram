#include "os/tc.h"
#include "os/core.h"
#include "logger.h"
#include <pthread.h>
#include <assert.h>
#include <vector>
#include <iostream>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

class Lock {
public:
    Lock() { pthread_mutex_lock(&mutex); }
    ~Lock() { pthread_mutex_unlock(&mutex); }
};

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
            pthread_cond_wait(&condition, &mutex);
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
};

// all posix threads share this mutex and this condition to implement cooperative TC threads
Condition condition;

// there is one posix thread running the core and one posix thread per TC thread
// this vector only manages the TC threads
std::vector<pthread_t*> threads;

void tc_init()
{
    assert (threads.empty());

    Logger::init();
    os_init();

    pthread_mutex_lock(&mutex);
}

static void* run_thread(void* ctx);

void tc_run_thread(void(*thread_start_function)())
{
    pthread_t* thread = new pthread_t();
    threads.push_back(thread);
    pthread_create(thread, 0, run_thread, (void*)thread_start_function);
    // let the TC thread hit the first yield point before spawning new threads or entering the core
    condition.wait();
}

static void* run_thread(void* ctx)
{
    void(*thread_start_function)() = (void(*)()) ctx;

    int old;
    int result;
    result = pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &old);
    assert (result == 0);

    result = pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old);
    assert (result == 0);

    { 
        Lock l;
        thread_start_function();
    }   
    return 0;
}

void tc_run()
{
    os_run();
    pthread_mutex_unlock(&mutex);
    for (size_t i=0; i<threads.size(); ++i) {
        pthread_cancel(*threads[i]);
        pthread_join(*threads[i], NULL);
    }
}

class DefaultContext : public LogContext {
public:
    DefaultContext(const std::string& syscall) : 
        LogContext(syscall)
    { }

    LogLine logReturn()
    {
        return LogContext::logReturn()(result);
    }

    // wait for completion of specific syscall
    Condition condition;
    error_t result;  
};

// called by core
void defaultCallback(void* ctx, error_t result)
{
    DefaultContext* context = (DefaultContext*) ctx;
    context->result = result;
    // let the blocking TC thread continue
    context->condition.signal();
    // wait for next syscall before resuming core
    condition.wait();
}

error_t tc_sleep(uint32_t ms)
{
    DefaultContext ctx("sleep");
    ctx.logCall()(ms);
    os_sleep(&defaultCallback, &ctx, ms);  
    condition.signal();
    ctx.condition.wait();
    ctx.logReturn();
    return ctx.result;
}

class ReceiveContext : public DefaultContext {
public:
    ReceiveContext(const std::string& name) : DefaultContext(name) { }
    size_t len; 
};

void receiveCallback(void* ctx, error_t result, size_t len)
{
    ((ReceiveContext*) ctx)->len = len;
    defaultCallback(ctx, result);
}

error_t tc_receive(int handle, uint8_t* buffer, size_t buflen, size_t* len)
{
    ReceiveContext ctx("receive");
    ctx.logCall()(handle)(buflen);
    os_receive(&receiveCallback, &ctx, handle, buffer, buflen);
    condition.signal();
    ctx.condition.wait();
    *len = ctx.len;
    ctx.logReturn()(array(buffer, *len));
    return ctx.result;
}

error_t tc_send(int handle, uint8_t* buffer, size_t len)
{
    DefaultContext ctx("send");
    ctx.logCall()(handle)(array(buffer, len));
    os_send(&defaultCallback, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    ctx.logReturn();
    return ctx.result;
}

error_t tc_flash_read(int handle, uint8_t* buffer, size_t buflen, size_t* len)
{
    ReceiveContext ctx("flash_read");
    ctx.logCall()(handle)(buflen);
    os_flash_read(&receiveCallback, &ctx, handle, buffer, buflen);
    condition.signal();
    ctx.condition.wait();
    *len = ctx.len;
    ctx.logReturn()(array(buffer, *len));
    return ctx.result;
}

error_t tc_flash_write(int handle, uint8_t* buffer, size_t len)
{
    DefaultContext ctx("flash_write");
    ctx.logCall()(handle)(array(buffer, len));
    os_flash_write(&defaultCallback, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    ctx.logReturn();
    return ctx.result;
}

class SensorReadContext : public DefaultContext {
public:
    SensorReadContext(const std::string& name) : DefaultContext(name) { }
    sensor_val_t value;
};

void callbackSensorRead(void* ctx, error_t result, sensor_val_t value)
{
    ((SensorReadContext*)ctx)->value = value;
    defaultCallback(ctx, result);
}

error_t tc_sensor_read(int handle, sensor_val_t* value)
{
    SensorReadContext ctx("sensor_read");
    ctx.logCall()(handle);
    os_sensor_read(&callbackSensorRead, &ctx, handle);
    condition.signal();
    ctx.condition.wait();
    *value = ctx.value;
    ctx.logReturn()(*value);
    return ctx.result;
}

