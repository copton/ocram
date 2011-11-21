#include "ec.h"
#include "dispatcher.h"
#include "random.h"
#include "logger.h"

// common //
int handle = 0;

uint32_t os_now()
{
    return Dispatcher::instance->get_simulation_time();
}

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

// syscalls //
class Syscall : public DispatcherCallback {
public:
    void operator()()
    {
        callback();
        line.log(Dispatcher::instance->get_simulation_time());
        Logger::instance->log(line);
        delete this; 
    }

    LogLine& log(const std::string& name)
    {
        line.log(name);
        return line;
    }

protected:
    virtual void callback() =0;

private:
    LogLine line;
};

template<class T1, class T2>
class Syscall2 : public Syscall {
public:
    Syscall2(const Callback2& cb, const T1& o1, const T2& o2) :
        cb(cb), o1(o1), o2(o2)
    { }

protected:
    void callback()
    {
        cb(o1, o2);
    }

private:
    Callback2 cb;
    T1 o1;
    T2 o2;
};

template<class T1, class T2, class T3>
class Syscall3 : public Syscall {
public:
    Syscall3(const Callback3& cb, const T1& o1, const T2& o2, const T3& o3) :
        cb(cb), o1(o1), o2(o2), o3(o3)
    { }

protected:
    void callback()
    {
        cb(o1, o2, o3);
    }

private:
    Callback3 cb;
    T1 o1;
    T2 o2;
    T3 o3;
};

template<class T1, class T2> Syscall2<T1, T2>* syscall2(const Callback2& cb, const T1& o1, const T2& o2)
{
    return new Syscall2<T1, T2>(cb, o1, o2);
}

template<class T1, class T2, class T3> Syscall3<T1, T2, T3>* syscall3(const Callback3& cb, const T1& o1, const T2& o2, const T3& o3)
{
    return new Syscall3<T1, T2, T3>(cb, o1, o2, o3);
}

// ec syscall implementation //
void ec_sleep(Callback2 cb, void* ctx, uint32_t ms)
{
    Syscall* syscall = syscall2(cb, ctx, rnd.error());
    syscall->log("sleep")(ms);
    Dispatcher::instance->enqueue(ms, syscall);
}

void ec_receive(Callback3 cb, void* ctx, int handle, uint8_t* buffer, size_t buflen)
{
    const uint32_t len = rnd.integer(buflen);
    rnd.bytes(buffer, len);
    const uint32_t eta = rnd.integer(10, 1000); // TODO find a good value
    Syscall* syscall = syscall3(cb, ctx, rnd.error(), len);
    syscall->log("receive")(handle)(buflen)(array(buffer, len));
    Dispatcher::instance->enqueue(eta, syscall);
}

void ec_send(Callback2 cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    const uint32_t eta = rnd.integer(1, 5); // TODO find a good value
    Syscall* syscall = syscall2(cb, ctx, rnd.error());
    syscall->log("send")(handle)(array(buffer, len));
    Dispatcher::instance->enqueue(eta, syscall);
}

void ec_flash_read(Callback2 cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    rnd.bytes(buffer, len);
    const uint32_t eta = rnd.integer(1, 10); // TODO find a good value
    Syscall* syscall = syscall2(cb, ctx, rnd.error());
    syscall->log("flash_read")(handle)(len)(array(buffer, len));
    Dispatcher::instance->enqueue(eta, syscall);
}

void ec_flash_write(Callback2 cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    const uint32_t eta = rnd.integer(1, 7); // TODO find a good value
    Syscall* syscall = syscall2(cb, ctx, rnd.error());
    syscall->log("flash_write")(handle)(array(buffer, len));
    Dispatcher::instance->enqueue(eta, syscall);
}

void ec_sensor_read(Callback2 cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    rnd.bytes(buffer, len);
    const uint32_t eta = rnd.integer(1, 3); // TODO find a good value
    Syscall* syscall = syscall2(cb, ctx, rnd.error());
    syscall->log("sensor_read")(handle)(array(buffer, len));
    Dispatcher::instance->enqueue(eta, syscall);
}
