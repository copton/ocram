#include "ec.h"
#include "dispatcher.h"
#include "random.h"
#include "logger.h"

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

class IOBuffer {
public:
    IOBuffer(const std::string& name) : refCount(0), name(name) { }

    int refCount;
    const std::string name;

    struct Descriptor {
        Descriptor(Mode mode) :
          mode(mode)
        { }

        bool canRead() { return (mode & READ) != 0; }
        bool canWrite() { return (mode & WRITE) != 0; }

        Mode mode;
    };
};

class File : public IOBuffer {
public:
    File(const std::string& name) : IOBuffer(name) { }
    
    typedef std::vector<uint8_t> Data;
    Data data;

    struct Descriptor : public IOBuffer::Descriptor {
        Descriptor(Mode mode, File* file) :
            IOBuffer::Descriptor(mode),
            file(file), offset(0)
        { }

        File* file;
        size_t offset;

        error_t read(uint8_t* data, size_t buflen, size_t* len)
        {
            *len = 0;
            if (! canRead()) {
                return FAIL;
            }
            if (offset < 0) {
                return FAIL;
            }
            if (offset > file->data.size()) {
                return FAIL;
            }
            size_t start = offset;
            size_t i = 0;
            for(; i<buflen && offset < file->data.size(); ++i, ++offset) {
                data[i] = file->data[offset];
            }
            *len = i;
            return SUCCESS;
        }

        error_t write(uint8_t* data, size_t len)
        {
            if (! canWrite()) {
                return FAIL;
            }
            if (offset < 0) {
                return FAIL;
            }
            if (offset > file->data.size()) {
                return FAIL;
            }
            for (int i=0; i<file->data.size() - (offset + len); ++i) {
                file->data.push_back(0);
            }
            for (int i=0; i < len; ++i, ++offset) {
                file->data[offset] = data[i];
            }
        }

        error_t seek(int newOffset)
        {
            if (newOffset < 0) {
                return FAIL;
            }
            if (newOffset > file->data.size()) {
                return FAIL;
            }
            offset = newOffset;
            return SUCCESS;
        }
    };
};

class FileManager {
public:
    FileManager() : handle(0) 
    { }

    int open(const std::string& name, Mode mode)
    {
        Files::iterator file = files.find(name);
        if (file == files.end()) {
            file = files.insert(std::make_pair(name, File(name))).first;
        }
        file->second.refCount += 1;
        Descriptors::iterator descriptor = descriptors.insert(std::make_pair(handle++, File::Descriptor(mode, &file->second))).first;
        return descriptor->first;
    }

    void close(int handle)
    {
        Descriptors::iterator descriptor = descriptors.find(handle);
        assert(descriptor != descriptors.end());
        descriptor->second.file->refCount -= 1;
        if (descriptor->second.file->refCount == 0) {
            files.erase(descriptor->second.file->name);
        }
        descriptors.erase(descriptor);
    }

    error_t seek(int handle, int offset)
    {
        Descriptors::iterator descriptor = descriptors.find(handle);
        assert(descriptor != descriptors.end());
        return descriptor->second.seek(offset);
    }

    error_t write(int handle, uint8_t* data, size_t len)
    {
        Descriptors::iterator descriptor = descriptors.find(handle);
        if (descriptor == descriptors.end()) {
            return FAIL;
        }
        return descriptor->second.write(data, len);
    }

    error_t read(int handle, uint8_t* data, size_t buflen, size_t* len)
    {
        Descriptors::iterator descriptor = descriptors.find(handle);
        if (descriptor == descriptors.end()) {
            return FAIL;
        }
        return descriptor->second.read(data, buflen, len);
    }

private:
    typedef std::map<std::string, File> Files;
    Files files;
    typedef std::map<int, File::Descriptor> Descriptors;
    Descriptors descriptors;
    int handle;
} fileManager;

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
    return fileManager.open(std::string("__flash__") + address, mode);
}

error_t os_flash_seek(int handle, int offset)
{
    return fileManager.seek(handle, offset);
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

    Syscall* syscall = syscall2(cb, ctx, error);

    Dispatcher::instance->enqueue(ms, syscall);
}

void ec_receive(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t buflen)
{
    const error_t error = rnd.error();
    const uint32_t len = rnd.integer(1, buflen);
    rnd.string((char*)buffer, len);
    const uint32_t eta = rnd.integer(10, 1000); // TODO find a good value

    Syscall* syscall = syscall3(cb, ctx, rnd.error(), len);

    Dispatcher::instance->enqueue(eta, syscall);
}

void ec_send(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    const error_t error = rnd.error();
    const uint32_t eta = rnd.integer(1, 5); // TODO find a good value

    Syscall* syscall = syscall2(cb, ctx, error);

    Dispatcher::instance->enqueue(eta, syscall);
}

void ec_flash_read(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t buflen)
{
    size_t len;
    const error_t error = fileManager.read(handle, buffer, buflen, &len);
    uint32_t eta;
    if (error != SUCCESS) eta = 1;
    else eta = rnd.integer(1, 10); // TODO find a good value

    Syscall* syscall = syscall3(cb, ctx, error, len);

    Dispatcher::instance->enqueue(eta, syscall);
}

void ec_flash_write(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len)
{
    const error_t error = rnd.error();
    const uint32_t eta = rnd.integer(1, 7); // TODO find a good value

    Syscall* syscall = syscall2(cb, ctx, error);

    Dispatcher::instance->enqueue(eta, syscall);
}

void ec_sensor_read(SensorReadCallback cb, void* ctx, int handle)
{
    const error_t error = rnd.error();
    const sensor_val_t value = rnd.integer(100);
    const uint32_t eta = rnd.integer(1, 3); // TODO find a good value

    Syscall* syscall = syscall3(cb, ctx, error, value);

    Dispatcher::instance->enqueue(eta, syscall);
}
