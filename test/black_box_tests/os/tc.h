#ifndef HAIGHAIBIJAIHOOGAEPO
#define HAIGHAIBIJAIHOOGAEPO

#include "ocram.h"
#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

TC_BLOCKING void tc_sleep(uint32_t ms);
TC_BLOCKING error_t tc_receive(void* handle, unsigned char* buffer, uint8_t buflen, uint8_t* len);
TC_BLOCKING error_t tc_send(void* handle, unsigned char* buffer, uint8_t len);
TC_BLOCKING error_t tc_flash_read(void* handle, unsigned char* buffer, uint8_t buflen, uint8_t* len);
TC_BLOCKING error_t tc_flash_write(void* handle, unsigned char* buffer, uint8_t len);
TC_BLOCKING error_t tc_sensor_read(void* handle, unsigned char* buffer, uint8_t buflen, uint8_t* len);

#ifdef __cplusplus
}
#endif
#endif
