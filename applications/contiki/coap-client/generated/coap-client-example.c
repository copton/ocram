#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "contiki.h"
#include "contiki-net.h"

#include "dev/button-sensor.h"
#include "net/uip-debug.h"

#include "er-coap-07-engine.h"

/* TODO: This server address is hard-coded for Cooja. */
#define SERVER_NODE(ipaddr)   uip_ip6addr(ipaddr, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202) /* cooja2 */

#define LOCAL_PORT      UIP_HTONS(COAP_DEFAULT_PORT+1)
#define REMOTE_PORT     UIP_HTONS(COAP_DEFAULT_PORT)

#define TOGGLE_INTERVAL 10

PROCESS(coap_client_example, "COAP Client Example");
AUTOSTART_PROCESSES(&coap_client_example);

uip_ipaddr_t server_ipaddr;
static struct etimer et;

/* Example URIs that can be queried. */
#define NUMBER_OF_URLS 4
/* leading and ending slashes only for demo purposes, get cropped automatically when setting the Uri-Path */
char* service_urls[NUMBER_OF_URLS] = {".well-known/core", "/actuators/toggle", "battery/", "error/in//path"};
static int uri_switch = 0;

/* This function is will be passed to COAP_BLOCKING_REQUEST() to handle responses. */
void client_chunk_handler(void *response)
{
  const uint8_t *chunk;
  int len = coap_get_payload(response, &chunk);
  printf("|%.*s", len, (char *)chunk);
}


PROCESS_THREAD(coap_client_example, ev, data)
{
  PROCESS_BEGIN();
  printf("Wooot!\n");

  static coap_packet_t request[1]; /* This way the packet can be treated as pointer as usual. */
  SERVER_NODE(&server_ipaddr);

  /* receives all CoAP messages */
  coap_receiver_init();

  etimer_set(&et, TOGGLE_INTERVAL * CLOCK_SECOND);

  SENSORS_ACTIVATE(button_sensor);
  printf("Press a button to request %s\n", service_urls[uri_switch]);

  while(1) {
    PROCESS_YIELD();

    if (etimer_expired(&et)) {
      printf("--Toggle timer--\n");

      /* prepare request, TID is set by COAP_BLOCKING_REQUEST() */
      coap_init_message(request, COAP_TYPE_CON, COAP_POST, 0 );
      coap_set_header_uri_path(request, service_urls[1]);
      coap_set_payload(request, (uint8_t *)"Toggle!", 8);

      PRINT6ADDR(&server_ipaddr);
      printf(" : %u\n", UIP_HTONS(REMOTE_PORT));

      COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, client_chunk_handler);

      printf("\n--Done--\n");

      etimer_reset(&et);

    } else if (ev == sensors_event && data == &button_sensor) {

      /* send a request to notify the end of the process */

      coap_init_message(request, COAP_TYPE_CON, COAP_GET, 0);
      coap_set_header_uri_path(request, service_urls[uri_switch]);

      printf("--Requesting %s--\n", service_urls[uri_switch]);

      PRINT6ADDR(&server_ipaddr);
      printf(" : %u\n", UIP_HTONS(REMOTE_PORT));

      COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, client_chunk_handler);

      printf("\n--Done--\n");

      uri_switch = (uri_switch+1) % NUMBER_OF_URLS;
    }
  }

  PROCESS_END();
}
