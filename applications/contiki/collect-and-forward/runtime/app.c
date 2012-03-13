#include <stdint.h>
#include <stdio.h>
#include "tl/tl.h"
#include "clock.h"
#include "config.h"

uint8_t app_stack_0[100];
size_t app_stack_size_0 = sizeof(app_stack_0);

void app_thread_0() {
    clock_time_t now = clock_time();
    while(1) {
        now += DT_COLLECT;
        tl_sleep(now);
        printf("woot\n");
    }
}
