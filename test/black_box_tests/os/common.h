#ifndef AIFIEGHAEGAEMONGACOO
#define AIFIEGHAEGAEMONGACOO

#include "types.h"

uint32_t os_now();
int os_listen(const char* address);
int os_connect(const char* address);
int os_flash_open(const char* address, Mode mode);
void os_flash_seek(void* handle, int offset);
int os_sensor_open(const char* address);

#endif
