#ifndef UUJIZOYAEGAXEWAEPHIE
#define UUJIZOYAEGAXEWAEPHIE

#include "common.h"

typedef void (*thread_t)(void*);
void pal_init(int);
void pal_start_thread(thread_t);
void pal_run();

typedef struct {
    void * ec_cont;
    error_t ec_result;
    uint8_t * buffer;
    int handle;
    size_t len;
} ec_frame_tc_flash_write_t;

typedef struct {
    void * ec_cont;
    error_t ec_result;
    int handle;
    sensor_val_t * value;
} ec_frame_tc_sensor_read_t;

typedef struct {
    void * ec_cont;
    error_t ec_result;
    uint32_t ms;
} ec_frame_tc_sleep_t;

typedef struct {
    void * ec_cont;
    error_t ec_result;
    uint8_t * buffer;
    size_t buflen;
    int handle;
    size_t * len;
} ec_frame_tc_receive_t;

typedef struct {
    void * ec_cont;
    error_t ec_result;
    uint8_t * buffer;
    int handle;
    size_t len;
} ec_frame_tc_send_t;

typedef struct {
    void * ec_cont;
    error_t ec_result;
    uint8_t * buffer;
    size_t buflen;
    int handle;
    size_t * len;
} ec_frame_tc_flash_read_t;

void tc_sleep(ec_frame_tc_sleep_t*);
void tc_receive(ec_frame_tc_receive_t*);
void tc_send(ec_frame_tc_send_t*);
void tc_flash_read(ec_frame_tc_flash_read_t*);
void tc_flash_write(ec_frame_tc_flash_write_t*);
void tc_sensor_read(ec_frame_tc_sensor_read_t*);

#endif