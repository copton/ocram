#include "ec.h"
#include "dispatcher.h"
#include "random.h"
#include "logger.h"

uint32_t os_now()
{
    return Dispatcher::instance->get_simulation_time();
}

int handle = 0;

int os_listen(const char* address)
{
    return handle++;
}

int os_connect(const char* address)
{
    return handle++;
}

int os_flash_open(const char* address, const char* mode)
{
    return handle++;
}

void os_flash_seek(int handle, int offset)
{
    return;
}

int sensor_open(const char* address)
{
    return handle++;
}

class SyscallLogger : public DispatcherCallback {
public:
    SyscallLogger(const LogLine& line) : line(line) { }

    void operator()()
    {
        callback();
        line.log(Dispatcher::instance->get_simulation_time());
        Logger::instance->log(line);
        delete this; 
    }
protected:
    virtual void callback() =0;

private:
    LogLine line;
    SyscallLogger(const SyscallLogger&);
};

class DefaultSyscall : public SyscallLogger {
public:
    DefaultSyscall(const LogLine& line, DefaultCallback cb, void* ctx) :
        SyscallLogger(line),
        cb(cb), ctx(ctx)
    { }

protected:
    void callback()
    {
        cb(ctx, SUCCESS);
    }

private:
    DefaultCallback cb;
    void* ctx;
};

class ReadSyscall : public SyscallLogger{
public:
    ReadSyscall(const LogLine& line, ReadCallback cb, void* ctx, size_t len) :
        SyscallLogger(line),
        cb(cb), ctx(ctx)
    { }

protected:
    void callback()
    {
       cb(ctx, SUCCESS, len);
    }

private:
    ReadCallback cb;
    void* ctx;
    size_t len;
};

class WriteSyscall : public SyscallLogger {
public:
    WriteSyscall(const LogLine& line, WriteCallback cb, void* ctx) :
        SyscallLogger(line),
        cb(cb), ctx(ctx)
    { }

public:
    void callback()
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
    Dispatcher::instance->enqueue(ms, new DefaultSyscall(
        LogLine("sleep").log(ms), cb, ctx));
}

void ec_receive(ReadCallback cb, void* ctx, int handle, uint8_t* buffer, size_t buflen)
{
    const uint32_t len = rnd.integer(buflen);
    rnd.bytes(buffer, len);
    const uint32_t eta = rnd.integer(10, 1000); // TODO find a good value
    Dispatcher::instance->enqueue(eta, new ReadSyscall(
        LogLine("receive").log(handle).log(Array(buffer, buflen)),
        cb, ctx, len));
}

void ec_send(WriteCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    const uint32_t eta = rnd.integer(1, 5); // TODO find a good value
    Dispatcher::instance->enqueue(eta, new DispatcherWriteCallback(cb, ctx));
}

void ec_flash_read(ReadCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    rnd.bytes(buffer, len);
    const uint32_t eta = rnd.integer(1, 10); // TODO find a good value
    Dispatcher::instance->enqueue(eta, new DispatcherReadCallback(cb, ctx, len));
}

void ec_flash_write(WriteCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    const uint32_t eta = rnd.integer(1, 7); // TODO find a good value
    Dispatcher::instance->enqueue(eta, new DispatcherWriteCallback(cb, ctx));
}

void ec_sensor_read(ReadCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    rnd.bytes(buffer, len);
    const uint32_t eta = rnd.integer(1, 3); // TODO find a good value
    Dispatcher::instance->enqueue(eta, new DispatcherReadCallback(cb, ctx, len));
}
