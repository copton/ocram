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
