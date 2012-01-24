#include "../../../os/tc.h"
#include <stdbool.h>

#define TOGGLE_INTERVAL (10 * CLOCK_SECOND)

TC_RUN_THREAD void task_toggle()
{
    clock_time_t now = clock_time() * CLOCK_SECOND;
    while(true) {
        tc_sleep(now + TOGGLE_INTERVAL);
        now += TOGGLE_INTERVAL;
    }
}
