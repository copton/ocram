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

void client_chunk_handler(void *response)
{
    const uint8_t *chunk;
    int len = coap_get_payload(response, &chunk);
    printf("|%.*s", len, (char *)chunk);
}

PROCESS_THREAD(coap_client, ev, data)
{
    PROCESS_BEGIN();
    printf("thread address: -1: %p\n", process_current);

    static coap_packet_t request[1];
    uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202); /* cooja2 */

    /* receives all CoAP messages */
    coap_receiver_init();

    etimer_set(&et, TOGGLE_INTERVAL * CLOCK_SECOND);

    SENSORS_ACTIVATE(button_sensor);

    while(1) {
        PROCESS_YIELD();

        if (etimer_expired(&et)) {
            {
                coap_init_message(request, COAP_TYPE_CON, COAP_POST, 0 );
                coap_set_header_uri_path(request, "random/salt");
                char salt[7];
                int size = snprintf(salt, sizeof(salt), "%d", rand() % 0xffff);
                if (size >= sizeof(salt)) {
                    printf("ASSERT: " __FILE__ ":%d\n",  __LINE__);
                    break;
                }
                coap_set_payload(request, (uint8_t*)salt, size-1);

                printf("setting salt: %d\n", salt);
                COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, client_chunk_handler);
                printf("done\n");
            }

            {
                coap_init_message(request, COAP_TYPE_CON, COAP_GET, 0);
                coap_set_header_uri_path(request, "random");
                char query[11];
                int size = snprintf(query, sizeof(query), "len=%d", rand() % 200);
                if (size >= sizeof(query)) {
                    printf("ASSERT: " __FILE__ ":%d\n",  __LINE__);
                    break;
                } 
                coap_set_header_uri_query(request, query); 

                printf("query random values\n");
                COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, client_chunk_handler);
                printf("done\n");
            }

            etimer_reset(&et);
        }
    }

    PROCESS_END();
}
