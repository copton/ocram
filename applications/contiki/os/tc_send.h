#ifndef JEIHIPAECAHBIEGIEDEI
#define JEIHIPAECAHBIEGIEDEI

#include "ocram.h"
#include "net/uip.h"
#include <stdint.h>
#include <stddef.h>

TC_BLOCKING void tc_send(struct uip_udp_conn* conn, uip_ipaddr_t* addr, uint16_t rport, uint8_t* buffer, size_t len);

#endif
