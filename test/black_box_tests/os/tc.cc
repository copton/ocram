#include "tc.h"
#include "ec.h"
#include "logger.h"
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

    pthread_mutex_t mutex;
};

class Condition {
public:
    Condition(Mutex& mutex)
        : waiting(false), mutex(mutex)
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
    Mutex& mutex;
};


// there is one posix thread running the ec layer and one posix thread per TC thread
// this vector only manages the TC threads
std::vector<pthread_t*> threads;

// all posix threads share this mutex and this condition to implement cooperative TC threads
Mutex mutex;
Condition condition(mutex);

void tc_init()
{
    Logger::init();
    ec_init();

    mutex.enter();
    assert (threads.empty());
}

static void* run_thread(void* ctx);

int tc_run_thread(void(*thread_start_function)())
{
    pthread_t* thread = new pthread_t();
    threads.push_back(thread);
    pthread_create(thread, 0, run_thread, (void*)thread_start_function);
    // let the TC thread hit the first yield point before spawning new threads or entering the ec core
    condition.wait();
    return threads.size() - 1;
}

static void* run_thread(void* ctx)
{
    void(*thread_start_function)() = (void(*)()) ctx;
    mutex.enter(); // be a cooperative TC thread
    thread_start_function();
    mutex.leave();
    return 0;
}

void tc_run()
{
    ec_run();
    assert (! threads.empty());
    mutex.leave();
}

void tc_join_thread(int handle)
{
    assert (handle >= 0);
    assert ((size_t)handle < threads.size());
    if (threads[handle] == 0) {
        std::cerr << "warning: thread " << handle << " has already been joined." << std::endl;
    } else {
        pthread_join(*threads[handle], 0);
        delete threads[handle];
        threads[handle] = 0;
    }
}

class DefaultContext {
public:
    DefaultContext(const std::string& name) : 
        condition(mutex),
        name(name),
        count(counter++)
    { }

    // wait for completion of specific syscall
    Condition condition;
    error_t result;  

    std::string name;
    static int counter;
    const int count;

    LogLine logCall()
    {
        LogLine line;
        return line(os_now())("->")(count)(name);
    }

    LogLine logReturn()
    {
        LogLine line;
        return line(os_now())("<-")(count)(name)(result);
    }

    virtual ~DefaultContext() { }
};

int DefaultContext::counter = 0;

// called by ec core
void defaultCallback(void* ctx, error_t result)
{
    DefaultContext* context = (DefaultContext*) ctx;
    context->result = result;
    // let the blocking TC thread continue
    context->condition.signal();
    // wait for next syscall before resuming ec core
    condition.wait();
}

error_t tc_sleep(uint32_t ms)
{
    DefaultContext ctx("sleep");
    ctx.logCall()(ms);
    ec_sleep(&defaultCallback, &ctx, ms);  
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
    ec_receive(&receiveCallback, &ctx, handle, buffer, buflen);
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
    ec_send(&defaultCallback, &ctx, handle, buffer, len);
    condition.signal();
    ctx.condition.wait();
    ctx.logReturn();
    return ctx.result;
}

error_t tc_flash_read(int handle, uint8_t* buffer, size_t buflen, size_t* len)
{
    ReceiveContext ctx("flash_read");
    ctx.logCall()(handle)(buflen);
    ec_flash_read(&receiveCallback, &ctx, handle, buffer, buflen);
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
    ec_flash_write(&defaultCallback, &ctx, handle, buffer, len);
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
    ec_sensor_read(&callbackSensorRead, &ctx, handle);
    condition.signal();
    ctx.condition.wait();
    *value = ctx.value;
    ctx.logReturn()(*value);
    return ctx.result;
}

