#include "os/common.h"

uint32_t os_now() { return 0; }
int os_listen(const char* address) { return -1; }
int os_connect(const char* address) { return -1; }
int os_flash_open(const char* address, Mode mode) { return -1; }
int os_sensor_open(const char* address) { return -1; }
error_t os_flash_seek(int handle, int offset) { return SUCCESS; }

