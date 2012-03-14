#include <stdint.h>
#include <stdio.h>
#include "tl/tl.h"
#include "clock.h"
#include "config.h"

uint8_t app_stack_0[100];

void app_thread_0() {
    printf("app0 started\n");
    clock_time_t now = clock_time();
    while(1) {
        printf("app0 loop\n");
        now += DT_COLLECT;
        printf("app0 sleep\n");
        tl_sleep(now);
        printf("woot0\n");
    }
}

uint8_t app_stack_1[100];

void app_thread_1() {
    printf("app1 started\n");
    clock_time_t now = clock_time();
    while(1) {
        printf("app1 loop\n");
        now += DT_SEND;
        printf("app1 sleep\n");
        tl_sleep(now);
        printf("woot1\n");
    }
}

void tl_app_main() {
    tl_create_thread(&app_thread_0, app_stack_0, sizeof(app_stack_0));
    tl_create_thread(&app_thread_1, app_stack_1, sizeof(app_stack_1));
}
