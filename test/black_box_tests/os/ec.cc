#include "ec.h"
#include "dispatcher.h"
#include "random.h"
#include "logger.h"
#include "file.h"

#include <assert.h>

#include <map>
#include <utility>
#include <iostream>

void ec_init()
{
    Dispatcher::init();
}

void ec_run()
{
    Dispatcher::instance->run();
}

FileSystem flashFileSystem;

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

int os_flash_open(const char* address, Mode mode)
{
    return flashFileSystem.open(address, mode);
}

error_t os_flash_seek(int handle, int offset)
{
    return flashFileSystem.resolve(handle).seek(offset);
}

int os_sensor_open(const char* address)
{
    return handle++;
}

// syscalls //
class Syscall : public DispatcherCallback {
public:
    void operator()()
    {
        callback();
        delete this; 
    }

protected:
    virtual void callback() =0;

};

template<class CB, class T1, class T2>
class Syscall2 : public Syscall {
public:
    Syscall2(const CB& cb, const T1& o1, const T2& o2) :
        cb(cb), o1(o1), o2(o2)
    { }

protected:
    void callback()
    {
        cb(o1, o2);
    }

private:
    CB cb;
    T1 o1;
    T2 o2;
};

template<class CB, class T1, class T2, class T3>
class Syscall3 : public Syscall {
public:
    Syscall3(const CB& cb, const T1& o1, const T2& o2, const T3& o3) :
        cb(cb), o1(o1), o2(o2), o3(o3)
    { }

protected:
    void callback()
    {
        cb(o1, o2, o3);
    }

private:
    CB cb;
    T1 o1;
    T2 o2;
    T3 o3;
};

template<class CB, class T1, class T2> Syscall2<CB, T1, T2>* syscall2(const CB& cb, const T1& o1, const T2& o2)
{
    return new Syscall2<CB, T1, T2>(cb, o1, o2);
}

template<class CB, class T1, class T2, class T3> Syscall3<CB, T1, T2, T3>* syscall3(const CB& cb, const T1& o1, const T2& o2, const T3& o3)
{
    return new Syscall3<CB, T1, T2, T3>(cb, o1, o2, o3);
}

// ec syscall implementation //
void ec_sleep(DefaultCallback cb, void* ctx, uint32_t ms)
{
    const error_t error = rnd.error();

    Dispatcher::instance->enqueue(ms, syscall2(cb, ctx, error));
}

void ec_receive(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t buflen)
{
    const error_t error = rnd.error();
    const uint32_t len = rnd.integer(1, buflen);
    rnd.string((char*)buffer, len);
    const uint32_t eta = rnd.integer(10, 1000); // TODO find a good value

    Dispatcher::instance->enqueue(eta, syscall3(cb, ctx, error, len));
}

void ec_send(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    const error_t error = rnd.error();
    const uint32_t eta = rnd.integer(1, 5); // TODO find a good value

    Dispatcher::instance->enqueue(eta, syscall2(cb, ctx, error));
}

void ec_flash_read(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t buflen)
{
    size_t len;
    const error_t error = flashFileSystem.resolve(handle).read(buffer, buflen, &len);
    const uint32_t eta = rnd.integer(1, 10); // TODO find a good value

    Dispatcher::instance->enqueue(eta, syscall3(cb, ctx, error, len));
}

void ec_flash_write(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    const error_t error = flashFileSystem.resolve(handle).write(buffer, len);
    const uint32_t eta = rnd.integer(1, 7); // TODO find a good value

    Dispatcher::instance->enqueue(eta, syscall2(cb, ctx, error));
}

void ec_sensor_read(SensorReadCallback cb, void* ctx, int handle)
{
    const error_t error = rnd.error();
    const sensor_val_t value = rnd.integer(100);
    const uint32_t eta = rnd.integer(1, 3); // TODO find a good value

    Dispatcher::instance->enqueue(eta, syscall3(cb, ctx, error, value));
}
