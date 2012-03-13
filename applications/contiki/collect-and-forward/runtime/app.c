#include <stdint.h>
#include <stdio.h>
#include "tl/tl.h"
#include "clock.h"
#include "config.h"

uint8_t app_stack_0[100];
size_t app_stack_size_0 = sizeof(app_stack_0);

void app_thread_0() {
    printf("app started\n");
    clock_time_t now = clock_time();
    while(1) {
        printf("app loop\n");
        now += 10* DT_COLLECT;
        printf("app sleep\n");
        tl_sleep(now);
        printf("woot\n");
    }
}
