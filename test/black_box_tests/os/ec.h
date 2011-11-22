#ifndef SOHTHATHEEYOOMEUKIEX
#define SOHTHATHEEYOOMEUKIEX

#include "common.h"

typedef void (*DefaultCallback)(void*, error_t);
typedef void (*ReceiveCallback)(void*, error_t, size_t);
typedef void (*SensorReadCallback)(void*, error_t, sensor_val_t);

#ifdef __cplusplus
extern "C" {
#endif

void ec_sleep(DefaultCallback cb, void* ctx, uint32_t ms);
void ec_receive(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t buflen);
void ec_send(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void ec_flash_read(ReceiveCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void ec_flash_write(DefaultCallback cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void ec_sensor_read(SensorReadCallback cb, void* ctx, int handle);

#ifdef __cplusplus
}
#endif
#endif
