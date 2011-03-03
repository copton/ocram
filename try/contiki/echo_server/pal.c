#include "contiki.h"
#include "app.h"
#include "dev/serial-line.h"

PROCESS_NAME(echo_server);
AUTOSTART_PROCESSES(&echo_server);

PROCESS(echo_server, "echo server");

struct process* thread1;
struct process* thread2;
void* frames[2];

int get_threadid()
{
	if (PROCESS_CURRENT() == thread1) {
		return 0;
	}
	if (PROCESS_CURRENT() == thread2) {
		return 1;
	}
}

inline void* load_frame()
{
	return frames[get_threadid()];
}

inline void store_frame(void* frame)
{
	frames[get_threadid()] = frame;
}

void serial_receive(frame_serial_receive* frame)
{
	frame->waiting = 1;
	store_frame(frame);
}

void sleep(frame_sleep* frame, int seconds)
{
	etimer_set(&frame->et, CLOCK_SECOND * seconds);
	store_frame(frame);
}

PROCESS_THREAD(echo_server, ev, data)
{
	PROCESS_BEGIN();

	thread1 = PROCESS_CURRENT();

	userland(NULL, NULL);
	while(1) {
		PROCESS_WAIT_EVENT();
		if (ev == serial_line_event_message) {
			frame_serial_receive* frame = (frame_serial_receive*) load_frame();
            if (frame->waiting) {
                frame->ec_result = data;	
                userland(frame->ec_cont_frame, frame->ec_cont_label);
                frame->waiting = 0;
            }
		} else if (ev == PROCESS_EVENT_TIMER) {
			frame_sleep* frame = (frame_sleep*)load_frame();
			userland(frame->ec_cont_frame, frame->ec_cont_label);
		} else {
			continue;
		}
	}
	
	PROCESS_END();
}
