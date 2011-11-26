#include "common.h"
#include "pal.h"
#include "ec.h"
#include "logger.h"

void pal_init()
{
    ec_init();
    Logger::init();
}

void pal_run()
{
    ec_run();
}

static void sleep_cb(void* ctx, error_t result)
{
	ec_frame_tc_sleep_t* frame = (ec_frame_tc_sleep_t*) ctx;
	frame->ec_result = result;
	frame->ec_thread(frame->ec_cont);	
}

void tc_sleep(ec_frame_tc_sleep_t * frame)
{
	ec_sleep(&sleep_cb, frame, frame->ms);
}

static void receive_cb(void* ctx, error_t result, size_t len)
{
	ec_frame_tc_receive_t* frame = (ec_frame_tc_receive_t*) ctx;
	frame->ec_result = result;
	*(frame->len) = len;
	frame->ec_thread(frame->ec_cont);
}

void tc_receive(ec_frame_tc_receive_t * frame)
{
	ec_receive(&receive_cb, frame, frame->handle, frame->buffer, frame->buflen);
}

static void send_cb(void* ctx, error_t result)
{
	ec_frame_tc_send_t* frame = (ec_frame_tc_send_t*) ctx;
	frame->ec_result = result;
	frame->ec_thread(frame->ec_cont);
}

void tc_send(ec_frame_tc_send_t * frame)
{
	ec_send(&send_cb, frame, frame->handle, frame->buffer, frame->len);
}

static void flash_read_cb(void* ctx, error_t result, size_t len)
{
	ec_frame_tc_flash_read_t* frame = (ec_frame_tc_flash_read_t*) ctx;
	frame->ec_result = result;
    *(frame->len) = len;
	frame->ec_thread(frame->ec_cont);
}

void tc_flash_read(ec_frame_tc_flash_read_t* frame)
{
	ec_flash_read(&flash_read_cb, frame, frame->handle, frame->buffer, frame->buflen);
}

static void flash_write_cb(void* ctx, error_t result)
{
	ec_frame_tc_flash_write_t* frame = (ec_frame_tc_flash_write_t*) ctx;
	frame->ec_result = result;
	frame->ec_thread(frame->ec_cont);
}

void tc_flash_write(ec_frame_tc_flash_write_t * frame)
{
	ec_flash_write(&flash_write_cb, frame, frame->handle, frame->buffer, frame->len);
}

static void sensor_read_cb(void* ctx, error_t result, sensor_val_t value)
{
	ec_frame_tc_sensor_read_t* frame = (ec_frame_tc_sensor_read_t*) ctx;
	*(frame->value) = value;
    frame->ec_result = result;
	frame->ec_thread(frame->ec_cont);
}

void tc_sensor_read(ec_frame_tc_sensor_read_t * frame)
{
	ec_sensor_read(&sensor_read_cb, frame, frame->handle);
}
