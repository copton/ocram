#ifndef FOHDIECOVAZEEPAUNEEJ
#define FOHDIECOVAZEEPAUNEEJ

#include "clock.h"

void tl_app_main();

void tl_create_thread(void (*fcn)(), uint8_t* stack, size_t size);
void tl_sleep(clock_time_t tics);

#endif
