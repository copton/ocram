#include "os/core.h"

void os_sleep(DefaultCallback cb, void* ctx, uint32_t ms) { }
void os_receive(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t buflen) { }
void os_send(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len) { }
void os_flash_read(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len) { }
void os_flash_write(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len) { }
void os_sensor_read(SensorReadCallback cb, void* ctx, int handle) { }
void os_init() { }
void os_run() { }
