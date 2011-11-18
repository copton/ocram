#ifndef AIFIEGHAEGAEMONGACOO
#define AIFIEGHAEGAEMONGACOO

#include "types.h"

uint32_t os_now();
void* os_listen(const char* address);
void* os_connect(const char* address);
void* os_flash_open(const char* address, const char* mode);
void os_flash_seek(void* handle, int offset);
void* sensor_open(const char* address);

#endif
