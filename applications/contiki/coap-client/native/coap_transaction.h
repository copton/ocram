#ifndef PHIEQUIEPELUVEILAAHO
#define PHIEQUIEPELUVEILAAHO

typedef struct coap_transaction {
  struct coap_transaction *next;

  uint16_t tid;
  struct etimer retrans_timer;
  uint8_t retrans_counter;

  uip_ipaddr_t addr;
  uint16_t port;

  restful_response_handler callback;
  void *callback_data;

  uint16_t packet_len;
  uint8_t packet[COAP_MAX_PACKET_SIZE+1];
} coap_transaction_t;

#endif
