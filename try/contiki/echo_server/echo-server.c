#include "pal.h"
#include "app.h"
#include <string.h>

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
receive: strcpy(FRAME->buffer, FRAME->frames.receive.ec_result);
        printf("XXX %s\n", FRAME->buffer);
        FRAME->frames.sleep.ec_cont_frame = frame;
        FRAME->frames.sleep.ec_cont_label = &&wakeup;
        sleep(&FRAME->frames.sleep, 5);
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
	printf("TRACE 1: serial receive\n");
}

void sleep(frame_sleep* frame, int seconds)
{
	etimer_set(&frame->et, CLOCK_SECOND * seconds);
	store_frame(frame);
	printf("TRACE 2: sleep\n");
}

PROCESS_THREAD(echo_server, ev, data)
{
	printf("TRACE 0: I am back!\n");
	PROCESS_BEGIN();

	thread1 = PROCESS_CURRENT();

	userland(NULL, NULL);
	while(1) {
		printf("TRACE 3: waiting...\n");
  		PROCESS_WAIT_EVENT_UNTIL((ev == serial_line_event_message && data != NULL) || (ev == PROCESS_EVENT_TIMER));
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
			printf("TRACE 6: it's timer\n");
			frame_sleep* frame = (frame_sleep*)load_frame();
			userland(frame->ec_cont_frame, frame->ec_cont_label);
		} else {
			printf("TRACE 7: it's an asshole :-( %d\n", ev);
			continue;
		}
	}
	
	PROCESS_END();
}
