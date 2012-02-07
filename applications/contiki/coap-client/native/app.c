#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "contiki.h"
#include "contiki-net.h"
#include "net/uip-debug.h"

#include "dev/button-sensor.h"
#include "er-coap-07-engine.h"

#define TOGGLE_INTERVAL 10
#define REMOTE_PORT     UIP_HTONS(COAP_DEFAULT_PORT)

PROCESS(coap_client, "COAP Client");
AUTOSTART_PROCESSES(&coap_client);

uip_ipaddr_t server_ipaddr;
static struct etimer et;

/* This function is will be passed to COAP_BLOCKING_REQUEST() to handle responses. */
void client_chunk_handler(void *response)
{
  const uint8_t *chunk;
  int len = coap_get_payload(response, &chunk);
  printf("|%.*s", len, (char *)chunk);
}

PROCESS_THREAD(coap_client, ev, data)
{
  PROCESS_BEGIN();

  static coap_packet_t request[1];
  uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202) /* cooja2 */

  /* receives all CoAP messages */
  coap_receiver_init();

  etimer_set(&et, TOGGLE_INTERVAL * CLOCK_SECOND);

  SENSORS_ACTIVATE(button_sensor);

  while(1) {
    PROCESS_YIELD();

    if (etimer_expired(&et)) {
      printf("--Toggle timer--\n");

      /* prepare request, TID is set by COAP_BLOCKING_REQUEST() */
      coap_init_message(request, COAP_TYPE_CON, COAP_POST, 0 );
      coap_set_header_uri_path(request, "/random");
      coap_set_header_uri_query(request, "?salt=23");

      uip_debug_ipaddr_print(&server_ipaddr);
      printf(" : %u\n", UIP_HTONS(REMOTE_PORT));

      COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, client_chunk_handler);

      printf("\n--Done--\n");

      etimer_reset(&et);
    }
  }

  PROCESS_END();
}
