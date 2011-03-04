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
		const int tid = *((int*)label);
        switch (tid) {
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

#define JOB_SERIAL 1
#define JOB_TIMER 2

typedef struct {
	int select;
	void* frame;
	union {
		int timer_seconds;
	} info;
} job_t;

job_t jobs[2]; // generic
job_t* job;

void serial_receive(frame_serial_receive* frame)
{
	printf("TRACE 1: receive\n");
	job->select = JOB_SERIAL;
	job->frame = frame;
}

void timer_sleep(frame_sleep* frame, int seconds)
{
	printf("TRACE 2: sleep\n");
	job->select = JOB_TIMER;
	job->frame = frame;
	job->info.timer_seconds = seconds;
}

PROCESS_THREAD(echo_server, ev, data) // generic
{
	printf("TRACE 0: I am back!\n");
	PROCESS_BEGIN();

	static int tid = 0; // generic
	job = jobs + tid;
	userland(NULL, &tid);

	while(1) {
		if (job->select == JOB_SERIAL) {
			printf("TRACE: reading from serial\n");
			PROCESS_WAIT_EVENT_UNTIL(ev == serial_line_event_message && data != NULL);
			printf("TRACE: received something\n");
			job = jobs + tid;
			frame_serial_receive* frame = (frame_serial_receive*) job->frame;
			frame->ec_result = data;	
			userland(frame->ec_cont_frame, frame->ec_cont_label);
		} else if (job->select == JOB_TIMER) {
			printf("TRACE: going to sleep\n");
			frame_sleep* frame = NULL;
			frame = (frame_sleep*) job->frame;
			etimer_set(&frame->et, CLOCK_SECOND * job->info.timer_seconds);
			PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&frame->et));	
			printf("TRACE: wake up\n");
			job = jobs + tid;
			frame = (frame_sleep*) job->frame;
			userland(frame->ec_cont_frame, frame->ec_cont_label);
		} else {
			printf("öhm... nothing to do?\n");
		}
	}
	
	PROCESS_END();
}
