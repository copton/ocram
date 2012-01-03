#include "os/tc.h"

error_t tc_sleep(uint32_t ms) { return SUCCESS; }
TC_BLOCKING error_t tc_receive(int handle, uint8_t* buffer, size_t buflen, size_t* len) { return SUCCESS; }
TC_BLOCKING error_t tc_send(int handle, uint8_t* buffer, size_t len) { return SUCCESS; }
TC_BLOCKING error_t tc_flash_read(int handle, uint8_t* buffer, size_t buflen, size_t* len) { return SUCCESS; }
TC_BLOCKING error_t tc_flash_write(int handle, uint8_t* buffer, size_t len) { return SUCCESS; }
TC_BLOCKING error_t tc_sensor_read(int handle, sensor_val_t* value) { return SUCCESS; }

void tc_init() { }
void tc_run() { }

void tc_run_thread(void(*thread_start_function)()) { }
