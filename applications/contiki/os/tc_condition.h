#ifndef SHOHNGOUZAIPHOOLEEDE
#define SHOHNGOUZAIPHOOLEEDE

#include "ocram.h"
#include <stdbool.h>
#include "clock.h"
#include "condition.h"

TC_BLOCKING void tc_condition_wait(condition_t* cond);
TC_BLOCKING void tc_condition_time_wait(condition_t* cond, clock_time_t tics);
void tc_condition_signal(condition_t* cond);

#endif
