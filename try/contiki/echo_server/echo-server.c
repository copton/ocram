#include "pal.h"
#include "app.h"
#include <string.h>
#include <stdio.h>

typedef struct {
    char buffer[512];
    union {
        frame_serial_receive receive;
        frame_sleep sleep;
    } frames;
} frame_task_1;

#define FRAME ((frame_task_1*)frame)

frame_task_1 stack1;

void userland(void* frame, void* label)
{
	printf("userland(%p, %p)\n", frame, label);
    if (frame == NULL) {
        switch (get_threadid()) {
        case 0:
            frame = &stack1;
            label = &&start;
            break;
        }
    }
    goto *label;

start: printf("hallo\n");
        FRAME->frames.receive.ec_cont_frame = frame;
        FRAME->frames.receive.ec_cont_label = &&receive;
        serial_receive(&FRAME->frames.receive);
        return;
receive: strncpy(FRAME->buffer, FRAME->frames.receive.ec_result, 512);
        printf("XXX %s\n", FRAME->buffer);
        FRAME->frames.sleep.ec_cont_frame = frame;
        FRAME->frames.sleep.ec_cont_label = &&wakeup;
		timer_sleep(&FRAME->frames.sleep, 5);
		return;
wakeup: printf("XXX %s\n", FRAME->buffer);
        goto start;
}

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
	printf ("DEBUG 1: something is wrong...\n");
	return -1;
}

void* load_frame()
{
	const int id = get_threadid();
	void*const frame = frames[id];
	printf("load frame %d %p\n", id, frame);
	return frame;
}

void store_frame(void* frame)
{
	const int id = get_threadid();
	printf("store frame %d %p\n", id, frame);
	frames[id]= frame;
}

void serial_receive(frame_serial_receive* frame)
{
	printf("TRACE 1: serial receive\n");
	frame->waiting = 1;
	store_frame(frame);
}

void timer_sleep(frame_sleep* frame, int seconds)
{
	printf("TRACE 2: sleep\n");
	etimer_set(&frame->et, CLOCK_SECOND * seconds);
	store_frame(frame);
}

PROCESS_THREAD(echo_server, ev, data)
{
	printf("TRACE 0: I am back!\n");
	PROCESS_BEGIN();

	thread1 = PROCESS_CURRENT();

	userland(NULL, NULL);
	while(1) {
		printf("TRACE 3: waiting...\n");
  		PROCESS_WAIT_EVENT();
		printf("TRACE 4: wakeup\n");
		if (ev == serial_line_event_message && data != NULL) {
			printf("TRACE 5: it's serial\n");
			frame_serial_receive* frame = (frame_serial_receive*) load_frame();
			if (frame->waiting) {
				frame->ec_result = data;	
				userland(frame->ec_cont_frame, frame->ec_cont_label);
				frame->waiting = 0;
			}
		} else if (ev == PROCESS_EVENT_TIMER) {
			printf("TRACE 6: it's a timer\n");
			frame_sleep* frame = (frame_sleep*)load_frame();
			userland(frame->ec_cont_frame, frame->ec_cont_label);
		} else {
			printf("TRACE 7: it's something unknown :-( %d\n", ev);
			continue;
		}
	}
	
	PROCESS_END();
}
