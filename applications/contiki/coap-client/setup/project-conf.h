#ifndef AWOHTIERIBOOJEEVOORE
#define AWOHTIERIBOOJEEVOORE

#undef NETSTACK_CONF_RDC
#define NETSTACK_CONF_RDC     nullrdc_driver

#ifndef REST_MAX_CHUNK_SIZE
#define REST_MAX_CHUNK_SIZE    64
#endif

#ifndef COAP_MAX_OPEN_TRANSACTIONS
#define COAP_MAX_OPEN_TRANSACTIONS   4
#endif

/* Must be <= open transaction number. */
#ifndef COAP_MAX_OBSERVERS
#define COAP_MAX_OBSERVERS      COAP_MAX_OPEN_TRANSACTIONS
#endif

#endif
