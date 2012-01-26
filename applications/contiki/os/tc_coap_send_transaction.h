#ifndef IESHOHSHOHDUUPAHCHOO
#define IESHOHSHOHDUUPAHCHOO

#include "ocram.h"
#include "er-coap-07-transactions.h"
#include "er-coap-07-engine.h"

TC_BLOCKING coap_packet_t* tc_coap_send_transaction(struct coap_transaction* transaction);

#endif
