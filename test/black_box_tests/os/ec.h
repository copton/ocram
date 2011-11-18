#ifndef SOHTHATHEEYOOMEUKIEX
#define SOHTHATHEEYOOMEUKIEX

#include "common.h"

typedef void (*DefaultCallback)(void*, error_t);
typedef void (*ReadCallback)(void*, error_t, uint8_t);
typedef void (*WriteCallback)(void*, error_t)

#ifdef __cplusplus
extern "C" {
#endif

void ec_sleep(DefaultCallback cb, void* ctx, uint32_t ms);
void ec_receive(ReadCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t buflen);
void ec_send(WriteCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len);
void ec_flash_read(ReadCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len);
void ec_flash_write(WriteCallback cb, void* ctx, void* handle, unsigned char* buffer, uint8_t len);

#ifdef __cplusplus
}
#endif
#endif
