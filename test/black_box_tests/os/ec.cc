#include "ec.h"
#include "dispatcher.h"

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
}

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
}

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
    const uint32_t len = random_int(buflen);
    random_string(buffer, len);
    const uint32_t eta = random_int(1000); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherReadCallback(cb, ctx, len));
}

void ec_send(WriteCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len)
{
    const uint32_t eta = random_int(5); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherWriteCallback(cb, ctx));
}

void ec_flash_read(ReadCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len)
{
    random_string(buffer, len);
    const uint32_t eta = random_int(10); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherReadCallback(cb, ctx, len));
}

void ec_flash_write(WriteCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len)
{
    const uint32_t eta = random_int(7); // TODO find a good value
    dispatcher.enqueue(eta, new DispatcherWriteCallback(cb, ctx));
}
