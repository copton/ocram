#ifdef OCRAM_MODE
#define THREAD __attribute__((tc_thread))
#define BLOCK __attribute__((tc_api))
#else
#define THREAD
#define BLOCK
#endif

#include <stdint.h>

BLOCK void tc_sleep(uint32_t ms);
void foo(int);
unsigned int tc_time();
void tc_toggle_led_0();

THREAD void blink1()
{
    unsigned int now = tc_time();
    while(1) {
        now += 250;
        tc_sleep(now);
        tc_toggle_led_0();
    }
}
