#ifndef HAIGHAIBIJAIHOOGAEPO
#define HAIGHAIBIJAIHOOGAEPO

#include "ocram.h"
#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

TC_BLOCKING error_t tc_sleep(uint32_t ms);
TC_BLOCKING error_t tc_receive(int handle, uint8_t* buffer, size_t buflen, size_t* len);
TC_BLOCKING error_t tc_send(int handle, uint8_t* buffer, size_t len);
TC_BLOCKING error_t tc_flash_read(int handle, uint8_t* buffer, size_t buflen, size_t* len);
TC_BLOCKING error_t tc_flash_write(int handle, uint8_t* buffer, size_t len);
TC_BLOCKING error_t tc_sensor_read(int handle, sensor_val_t* value);

void tc_init();
void tc_run();

int tc_run_thread(void(*thread_start_function)());
void tc_join_thread(int handle);

#ifdef __cplusplus
}
#endif
#endif
