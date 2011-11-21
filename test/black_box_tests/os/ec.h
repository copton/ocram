#ifndef SOHTHATHEEYOOMEUKIEX
#define SOHTHATHEEYOOMEUKIEX

#include "common.h"

typedef void (*Callback2)(void*, error_t);
typedef void (*Callback3)(void*, error_t, size_t);

#ifdef __cplusplus
extern "C" {
#endif

void ec_sleep(Callback2 cb, void* ctx, uint32_t ms);
void ec_receive(Callback3 cb, void* ctx, int handle, uint8_t* buffer, size_t buflen);
void ec_send(Callback2 cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void ec_flash_read(Callback2 cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void ec_flash_write(Callback2 cb, void* ctx, int handle, uint8_t* buffer, size_t len);
void ec_sensor_read(Callback2 cb, void* ctx, int handle, uint8_t* buffer, size_t len);

#ifdef __cplusplus
}
#endif
#endif
