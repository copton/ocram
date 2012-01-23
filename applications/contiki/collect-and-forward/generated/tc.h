#ifndef OEQUEESHUYIGHOUNGUGH
#define OEQUEESHUYIGHOUNGUGH

#ifdef OCRAM_MODE
#define TC_RUN_THREAD __attribute__((tc_run_thread))
#define TC_BLOCKING __attribute__((tc_blocking))
#else
#define TC_RUN_THREAD
#define TC_BLOCKING
#endif

#include "types.h"

#include "clock.h"
#include "net/uip.h"

TC_BLOCKING error_t tc_sleep(clock_time_t tics);
TC_BLOCKING error_t tc_receive(uint8_t* buffer, size_t buflen, size_t* len);
TC_BLOCKING error_t tc_send(struct uip_udp_conn* conn, uip_ipaddr_t* addr, uint16_t rport, uint8_t* buffer, size_t len);

#endif
