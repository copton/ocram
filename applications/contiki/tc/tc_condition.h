#ifndef SHOHNGOUZAIPHOOLEEDE
#define SHOHNGOUZAIPHOOLEEDE

#include "ocram.h"
#include <stdbool.h>
#include "clock.h"
#include "condition.h"

TC_BLOCKING void tc_condition_wait(condition_t* cond);
void tc_condition_signal(condition_t* cond, void* data);

#endif
