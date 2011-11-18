#include "common.h"
#include "dispatcher.h"

uint32_t os_now()
{
    return dispatcher.simulation_time();
}

void* os_listen(const char* address)
{
    return address;
}

void* os_connect(const char* address)
{
    return address;
}

void* os_flash_open(const char* address, const char* mode)
{
    return address;
}

void os_flash_seek(void* handle, int offset)
{
    return address;
}

void* sensor_open(const char* address)
{
    return address;
}
