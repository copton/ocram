#ifndef OHTHAEVOHGOHDECHOOCH
#define OHTHAEVOHGOHDECHOOCH

#include "types.h"

typedef void (*DispatcherCallback)(void*);

void dispatcher_init();
void dispatcher_run();
bool dispatcher_quit();
void dispatcher_enqueue(int32_t callback_time, DispatcherCallback callback, void* ctx);
uint32_t dispatcher_now();

#endif
