#ifdef OCRAM_MODE
#define THREAD __attribute__((tc_run_thread))
#define BLOCK __attribute__((tc_blocking))
#else
#define THREAD
#define BLOCK
#endif

#include <stdint.h>

BLOCK void tc_sleep(uint32_t);
uint32_t tc_time();
void tc_toggle_led_0();
void tc_toggle_led_1();
void tc_toggle_led_2();

THREAD void blink1()
{
    uint32_t now = tc_time();
    while(1) {
        now += 250;
        tc_sleep(now);
        tc_toggle_led_0();
    }
}

THREAD void blink2()
{
    uint32_t now = tc_time();
    while(1) {
        now += 500;
        tc_sleep(now);
        tc_toggle_led_1();
    }
}

THREAD void blink3()
{
    uint32_t now = tc_time();
    while(1) {
        now += 1000;
        tc_sleep(now);
        tc_toggle_led_2();
    }
}
