#include "tc.h"

void log_to(void* handle, uint8_t* buf, int len);

void collect_run(const char* sensor, const char* file, unsigned dt)
{
    void* sensor_handle = os_sensor_open(sensor);
    void* logw_handle = os_flash_open(file, "w")

    uint32_t now = os_now();
    while (true) {
        tc_sleep(timer_handle, now + dt);
        now += dt;

        sensor_val_t val;
		error_t res = sensor_read(sensor_handle, &val);
		assert (res == SUCCESS);

        log_to(logw_handle, &val, sizeof(sensor_val_t));
    }
}

void receive_run(const char* channel, const char* file)
{
    void* socket = os_listen(channel);
    void* log = os_flash_open(file, "r")
    while (true) {
        uint8_t buffer[1024];
        size_t len;

		error_t res = tc_receive(socket, buffer, 1024, &len);
		assert (res == SUCCESS);

		log_to(log, buffer, len);
    }
}

static void aggregate_from(void* handle, int32_t* min, int32_t* max)
{
    uint8_t _buffer[1024];
    int len;

    error_t res = logr_read(handle, read_buffer, sizeof(read_buffer), &len);
    assert (res == SUCCESS);
    assert((len % sizeof(int32_t)) == 0);

    { int i;
        for (i=0; i< len / sizeof(int32_t); i++) {
            int32_t tmp;
            memcpy(&tmp, read_buffer + i * sizeof(int32_t), sizeof(int32_t));
            if (tmp < *min) *min = tmp;
            if (tmp > *max) *max = tmp;
        }
    }
}

static void send_via(void* handle, int32_t min, int32_t max)
{
    net_message_t msg;
    void* payload = send_getPayload(handle, &msg, 2 * sizeof(int32_t));
    memcpy(payload, &min, sizeof(int32_t));
    memcpy(payload + sizeof(int32_t), &max, sizeof(int32_t));

    error_t res = send_send(handle, &msg, 2 * sizeof(int32_t));
    assert(res == SUCCESS);
}

void send_run(const char* channel, const char* file1, const char* file2, unsigned dt)
{
    void* logr1_handle = logr_wire(file1);
    void* logr2_handle = logr_wire(file2);
    void* send_handle = send_wire(channel);
	void* timer_handle = timer_wire();
    uint32_t now = timer_getNow(timer_handle);

    while (true) {
        timer_sleep(timer_handle, now + dt);
        now += dt;

        int32_t min = 0x7FFFFFFF;
        int32_t max = 0xFFFFFFFF;

        aggregate_from(logr1_handle, &min, &max);
        aggregate_from(logr2_handle, &min, &max);

        send_via(send_handle, min, max); 
    } 
}

void log_to(void* handle, void* buf, storage_len_t len)
{
    storage_len_t res_len;
    bool recordsLost;
    error_t res = logw_append(handle, buf, len, &res_len, &recordsLost);
    assert (res == SUCCESS);
    assert (res_len == len);
}

TC_RUN_THREAD void task_send()
{
	send_run(fn_send, fn_flash_receive_source, fn_flash_collect_source, dt_send);
	assert (false);
}

TC_RUN_THREAD void run_receive()
{
	receive_run(fn_receive, fn_flash_receive_sink);
	assert (false);
}

TC_RUN_THREAD void run_collect()
{
	collect_run(fn_collect, fn_flash_collect_sink, dt_collect);
	assert (false);
}
