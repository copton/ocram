#include "os/pal.h"

void pal_init(int n) { }
void pal_start_thread(thread_t t) { }
void pal_run() { }

void tc_sleep(ec_frame_tc_sleep_t* f) { }
void tc_receive(ec_frame_tc_receive_t* f) { }
void tc_send(ec_frame_tc_send_t* f) { }
void tc_flash_read(ec_frame_tc_flash_read_t* f) { }
void tc_flash_write(ec_frame_tc_flash_write_t* f) { }
void tc_sensor_read(ec_frame_tc_sensor_read_t* f) { }
