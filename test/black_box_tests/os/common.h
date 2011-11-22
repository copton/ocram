#ifndef AIFIEGHAEGAEMONGACOO
#define AIFIEGHAEGAEMONGACOO

#include "types.h"

#ifdef __cplusplus
extern "C" {
#endif

uint32_t os_now();
int os_listen(const char* address);
int os_connect(const char* address);
int os_flash_open(const char* address, Mode mode);
int os_sensor_open(const char* address);

error_t os_flash_seek(int handle, int offset);

#ifdef __cplusplus
}
#endif

#endif
