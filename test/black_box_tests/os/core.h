#ifndef SOHTHATHEEYOOMEUKIEX
#define SOHTHATHEEYOOMEUKIEX

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*DefaultCallback)(void*, error_t);
typedef void (*ReceiveCallback)(void*, error_t, size_t);
typedef void (*SensorReadCallback)(void*, error_t, sensor_val_t);

void os_sleep(DefaultCallback cb, void* ctx, uint32_t ms);
void os_receive(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t buflen);
void os_send(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void os_flash_read(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void os_flash_write(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void os_sensor_read(SensorReadCallback cb, void* ctx, int handle);

void os_init();
void os_run();

#ifdef __cplusplus
}
#endif

#endif
