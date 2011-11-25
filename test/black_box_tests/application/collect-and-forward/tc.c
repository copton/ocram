#include "os/tc.h"
#include <assert.h>

void *memcpy(void *dest, const void *src, size_t n);

void log_to(int log, uint8_t* buf, size_t len);

void collect_run(const char* device, const char* file, unsigned dt)
{
    int sensor = os_sensor_open(device);
    int log = os_flash_open(file, WRITE);

    uint32_t now = os_now();
    while (true) {
        tc_sleep(now + dt);
        now += dt;

        sensor_val_t val;
		while (tc_sensor_read(sensor, &val) != SUCCESS);

        log_to(log, (uint8_t*)&val, sizeof(sensor_val_t));
    }
}

void receive_run(const char* channel, const char* file)
{
    int socket = os_listen(channel);
    int log = os_flash_open(file, WRITE);
    while (true) {
        uint8_t buffer[10];
        size_t len;

		while(tc_receive(socket, buffer, sizeof(buffer), &len) != SUCCESS);
        len -= len % sizeof(int32_t);

		log_to(log, buffer, len);
    }
}

static void aggregate_from(int log, int32_t* min, int32_t* max)
{
    uint8_t buffer[1024];
    size_t len;

	error_t result = tc_flash_read(log, buffer, sizeof(buffer), &len);
	assert(result == SUCCESS);
    assert((len % sizeof(int32_t)) == 0);
	assert(len < sizeof(buffer));

	result = os_flash_seek(log, 0);
	assert (result == SUCCESS);

	int i;
	for (i=0; i < len / sizeof(int32_t); i++) {
		int32_t tmp;
		memcpy(&tmp, buffer + i * sizeof(int32_t), sizeof(int32_t));
		if (tmp < *min) *min = tmp;
		if (tmp > *max) *max = tmp;
	}
}

static void send_via(int socket, int32_t min, int32_t max)
{
	uint8_t payload[2 * sizeof(int32_t)];
    memcpy(payload, &min, sizeof(int32_t));
    memcpy(payload + sizeof(int32_t), &max, sizeof(int32_t));

	while(tc_send(socket, payload, sizeof(payload)) != SUCCESS);
}

void send_run(const char* channel, const char* file1, const char* file2, unsigned dt)
{
    int log1 = os_flash_open(file1, READ);
    int log2 = os_flash_open(file2, READ);
    int socket = os_connect(channel);
    uint32_t now = os_now();

    while (true) {
        tc_sleep(now + dt);
        now += dt;

        int32_t min = 0x7FFFFFFF;
        int32_t max = 0xFFFFFFFF;

        aggregate_from(log1, &min, &max);
        aggregate_from(log2, &min, &max);

        send_via(socket, min, max); 
    } 
}

void log_to(int log, uint8_t* buf, size_t len)
{
	while(tc_flash_write(log, buf, len) != SUCCESS);
}

TC_RUN_THREAD void task_send()
{
	send_run("parent", "receive_log", "sensor_log", 100);
	assert (false);
}

TC_RUN_THREAD void task_receive()
{
	receive_run("child", "receive_log");
	assert (false);
}

TC_RUN_THREAD void task_collect()
{
	collect_run("sensor", "sensor_log", 50);
	assert (false);
}
