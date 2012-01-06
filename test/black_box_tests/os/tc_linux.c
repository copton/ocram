#include "tc.h"

#include "core.h"

#include <pthread.h>
#include <assert.h>

#define MAX_THREADS 5

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

typedef struct {
    bool waiting;
    pthread_cond_t condition;
} Condition;

static void cond_init(Condition* cond)
{
    pthread_cond_init(&cond->condition, NULL);
    cond->waiting = false;
}

static void cond_wait(Condition* cond)
{
    cond->waiting = true;
    while (cond->waiting) {
        pthread_cond_wait(&cond->condition, &mutex);
    }
}

static void cond_signal(Condition* cond)
{
    cond->waiting = false;
    pthread_cond_signal(&cond->condition);
}

// all posix threads share this mutex and this condition to implement cooperative TC threads
Condition condition;

// there is one posix thread running the core and one posix thread per TC thread
// this vector only manages the TC threads
pthread_t threads[MAX_THREADS];
size_t numberof_threads;

void tc_init()
{
    numberof_threads = 0;
    cond_init(&condition);
    os_init();

    pthread_mutex_lock(&mutex);
}

static void* run_thread(void* ctx);

void tc_run_thread(void(*thread_start_function)())
{
    assert (numberof_threads < MAX_THREADS);
    pthread_create(&threads[numberof_threads++], 0, run_thread, (void*)thread_start_function);
    // let the TC thread hit the first yield point before spawning new threads or entering the core
    cond_wait(&condition);
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

    pthread_mutex_lock(&mutex);
    thread_start_function();
    pthread_mutex_unlock(&mutex);

    return 0;
}

void tc_run()
{
    os_run();
    pthread_mutex_unlock(&mutex);
    for (size_t i=0; i<numberof_threads; ++i) {
        pthread_cancel(threads[i]);
        pthread_join(threads[i], NULL);
    }
}

typedef struct {
    // wait for completion of specific syscall
    Condition condition;
    error_t result;  
} DefaultContext;

// called by core
void defaultCallback(void* ctx, error_t result)
{
    DefaultContext* context = (DefaultContext*) ctx;
    context->result = result;
    // let the blocking TC thread continue
    cond_signal(&context->condition);
    // wait for next syscall before resuming core
    cond_wait(&condition);
}

typedef struct {
    // wait for completion of specific syscall
    Condition condition;
    error_t result;  
    size_t len; 
} ReceiveContext;

void receiveCallback(void* ctx, error_t result, size_t len)
{
    ((ReceiveContext*) ctx)->len = len;
    defaultCallback(ctx, result);
}

typedef struct {
    // wait for completion of specific syscall
    Condition condition;
    error_t result;  
    sensor_val_t value;
} SensorReadContext;

void callbackSensorRead(void* ctx, error_t result, sensor_val_t value)
{
    ((SensorReadContext*)ctx)->value = value;
    defaultCallback(ctx, result);
}


error_t tc_sleep(uint32_t ms)
{
    DefaultContext ctx;
    os_sleep(&defaultCallback, &ctx, ms);  
    cond_signal(&condition);
    cond_wait(&ctx.condition);
    return ctx.result;
}

error_t tc_receive(int handle, uint8_t* buffer, size_t buflen, size_t* len)
{
    ReceiveContext ctx;
    cond_init(&ctx.condition);
    os_receive(&receiveCallback, &ctx, handle, buffer, buflen);
    cond_signal(&condition);
    cond_wait(&ctx.condition);
    *len = ctx.len;
    return ctx.result;
}

error_t tc_send(int handle, uint8_t* buffer, size_t len)
{
    DefaultContext ctx;
    cond_init(&ctx.condition);
    os_send(&defaultCallback, &ctx, handle, buffer, len);
    cond_signal(&condition);
    cond_wait(&ctx.condition);
    return ctx.result;
}

error_t tc_flash_read(int handle, uint8_t* buffer, size_t buflen, size_t* len)
{
    ReceiveContext ctx;
    cond_init(&ctx.condition);
    os_flash_read(&receiveCallback, &ctx, handle, buffer, buflen);
    cond_signal(&condition);
    cond_wait(&ctx.condition);
    *len = ctx.len;
    return ctx.result;
}

error_t tc_flash_write(int handle, uint8_t* buffer, size_t len)
{
    DefaultContext ctx;
    cond_init(&ctx.condition);
    os_flash_write(&defaultCallback, &ctx, handle, buffer, len);
    cond_signal(&condition);
    cond_wait(&ctx.condition);
    return ctx.result;
}

error_t tc_sensor_read(int handle, sensor_val_t* value)
{
    SensorReadContext ctx;
    cond_init(&ctx.condition);
    os_sensor_read(&callbackSensorRead, &ctx, handle);
    cond_signal(&condition);
    cond_wait(&ctx.condition);
    *value = ctx.value;
    return ctx.result;
}
