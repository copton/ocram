#include "ec.h"
#include "dispatcher.h"
#include "random.h"

uint32_t os_now()
{
    return dispatcher.get_simulation_time();
}

void* os_listen(const char* address)
{
    return (void*) address;
}

void* os_connect(const char* address)
{
    return (void*) address;
}

void* os_flash_open(const char* address, const char* mode)
{
    return (void*) address;
}

void os_flash_seek(void* handle, int offset)
{
    return;
}

void* sensor_open(const char* address)
{
    return (void*) address;
}

class DispatcherDefaultCallback : public DispatcherCallback {
public:
    DispatcherDefaultCallback(DefaultCallback cb, void* ctx) :
        cb(cb), ctx(ctx)
    { }

    void operator()()
    {
       cb(ctx, SUCCESS);
       delete this; 
    }

private:
    DefaultCallback cb;
    void* ctx;
};

class DispatcherReadCallback : public DispatcherCallback {
public:
    DispatcherReadCallback(ReadCallback cb, void* ctx, uint8_t len) :
        cb(cb), ctx(ctx), len(len)
    { }

    void operator()()
    {
       cb(ctx, SUCCESS, len);
       delete this; 
    }

private:
    ReadCallback cb;
    void* ctx;
    uint8_t len;
};

class DispatcherWriteCallback : public DispatcherCallback {
public:
    DispatcherWriteCallback(WriteCallback cb, void* ctx) :
        cb(cb), ctx(ctx)
    { }

    void operator()()
    {
        cb(ctx, SUCCESS);
        delete this;
    }

private:
    WriteCallback cb;
    void* ctx;
};

void ec_sleep(DefaultCallback cb, void* ctx, uint32_t ms)
{
    dispatcher.enqueue(ms, new DispatcherDefaultCallback(cb, ctx));
}

void ec_receive(ReadCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t buflen)
{
    const uint32_t len = rnd.integer(buflen);
    rnd.string(buffer, len);
    const uint32_t eta = rnd.integer(1000); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherReadCallback(cb, ctx, len));
}

void ec_send(WriteCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len)
{
    const uint32_t eta = rnd.integer(5); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherWriteCallback(cb, ctx));
}

void ec_flash_read(ReadCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len)
{
    rnd.string(buffer, len);
    const uint32_t eta = rnd.integer(10); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherReadCallback(cb, ctx, len));
}

void ec_flash_write(WriteCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len)
{
    const uint32_t eta = rnd.integer(7); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherWriteCallback(cb, ctx));
}

void ec_sensor_read(ReadCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len)
{
    rnd.string(buffer, len);
    const uint32_t eta = rnd.integer(3); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherReadCallback(cb, ctx, len));
}
