#ifndef FOHDIECOVAZEEPAUNEEJ
#define FOHDIECOVAZEEPAUNEEJ

#include "clock.h"
#include "contiki-net.h"
#include "net/uip.h"
#include <stdbool.h>

void tl_app_main();

void tl_create_thread(void (*fcn)(), uint8_t* stack, size_t size);
void tl_sleep(clock_time_t tics);
void tl_receive();
void tl_send(struct uip_udp_conn* conn, uip_ipaddr_t* addr, uint16_t rport, uint8_t* buffer, size_t len);

typedef struct {
    void* waiting_thread;
    bool waiting;
} condition_t;

#define TL_CONDITION_INITIALIZER {NULL, false}

void tl_condition_init(condition_t* cond);
void tl_condition_wait(condition_t* cond);

// the thread library runs one contiki thread for all application threads. So
// it can not know for which timers this thread is waiting for. That's why we
// have to tell it.
bool tl_condition_time_wait(condition_t* cond, struct etimer** timers, size_t numberofTimers) ;
void tl_condition_signal(condition_t* cond);

#endif
