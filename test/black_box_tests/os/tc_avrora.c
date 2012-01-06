#include "tc.h"

TC_BLOCKING error_t tc_sleep(uint32_t ms) { return FAIL; }
TC_BLOCKING error_t tc_receive(int handle, uint8_t* buffer, size_t buflen, size_t* len) { return FAIL; }
TC_BLOCKING error_t tc_send(int handle, uint8_t* buffer, size_t len) { return FAIL; }
TC_BLOCKING error_t tc_flash_read(int handle, uint8_t* buffer, size_t buflen, size_t* len) { return FAIL; }
TC_BLOCKING error_t tc_flash_write(int handle, uint8_t* buffer, size_t len) { return FAIL; }
TC_BLOCKING error_t tc_sensor_read(int handle, sensor_val_t* value) { return FAIL; }

void tc_init() { }
void tc_run() { }

void tc_run_thread(void(*thread_start_function)()) { }

#endif
