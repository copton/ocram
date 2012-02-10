#include <stdio.h>
#include <stdlib.h>

#include "contiki.h"
#include "er-coap-07-engine.h"

#include "sim_assert.h"

#define TOGGLE_INTERVAL (10 * CLOCK_SECOND)
#define REMOTE_PORT     UIP_HTONS(COAP_DEFAULT_PORT)

void client_chunk_handler(void *response)
{
    coap_packet_t* packet = response;
    const uint8_t *chunk;
    int len = coap_get_payload(response, &chunk);
    printf("coap response: %d.%02d: %.*s\n", packet->code/32, packet->code % 32, len, (char *)chunk);
}

// rand is not standard compliant on this platform and sometimes returns negative values
int rand_fixed()
{
    int value;
    do {
        value = rand();
    } while (value < 0);
    return value;
}

PROCESS(coap_client, "COAP Client");
AUTOSTART_PROCESSES(&coap_client);

struct my_request_state_t {
    struct pt pt;
    struct process *process;
    coap_transaction_t *transaction;
    coap_packet_t *response;
    uint32_t block_num;
};

void my_blocking_request_callback(void *callback_data, void *response) {
    struct request_state_t *state = (struct request_state_t *) callback_data;
    state->response = (coap_packet_t*) response;
    process_poll(state->process);
}

PT_THREAD(my_coap_blocking_request(struct request_state_t *state, process_event_t ev, uip_ipaddr_t *remote_ipaddr, uint16_t remote_port, coap_packet_t *request, blocking_response_handler request_callback))
{
    PT_BEGIN(&state->pt);

    static uint8_t more;
    static uint32_t res_block;
    static uint8_t block_error;

    state->block_num = 0;
    state->response = NULL;
    state->process = PROCESS_CURRENT();

    more = 0;
    res_block = 0;
    block_error = 0;

    do {
        request->tid = coap_get_tid();
        if ((state->transaction = coap_new_transaction(request->tid, remote_ipaddr, remote_port)))
        {
            state->transaction->callback = my_blocking_request_callback;
            state->transaction->callback_data = state;

            if (state->block_num>0)
            {
                coap_set_header_block2(request, state->block_num, 0, REST_MAX_CHUNK_SIZE);
            }

            state->transaction->packet_len = coap_serialize_message(request, state->transaction->packet);

            coap_send_transaction(state->transaction);
            printf("Requested #%lu (TID %u)\n", state->block_num, request->tid);

            PT_YIELD_UNTIL(&state->pt, ev == PROCESS_EVENT_POLL);

            if (!state->response)
            {
                printf("Server not responding\n");
                PT_EXIT(&state->pt);
            }

            coap_get_header_block2(state->response, &res_block, &more, NULL, NULL);

            printf("Received #%lu%s (%u bytes)\n", res_block, more ? "+" : "", state->response->payload_len);

            if (res_block==state->block_num)
            {
                request_callback(state->response);
                ++(state->block_num);
            }
            else
            {
                printf("WRONG BLOCK %lu/%lu\n", res_block, state->block_num);
                ++block_error;
            }
        }
        else
        {
            printf("Could not allocate transaction buffer");
            PT_EXIT(&state->pt);
        }
    } while (more && block_error<COAP_MAX_ATTEMPTS);

    PT_END(&state->pt);
}

#define MY_COAP_BLOCKING_REQUEST(server_addr, server_port, request) \                                                                                                                                                   
    static struct my_request_state_t request_state; \
    PT_SPAWN(process_pt, &request_state.pt, my_coap_blocking_request(&request_state, ev, server_addr, server_port, request));

PROCESS_THREAD(coap_client, ev, data)
{
    static uip_ipaddr_t server_ipaddr;
    static coap_packet_t request[1];
    static struct etimer et;

    PROCESS_BEGIN();
    printf("thread address: 0: %p\n", process_current);

    coap_receiver_init();
    etimer_set(&et, TOGGLE_INTERVAL);
    uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202); /* cooja2 */

    while(1) {
        PROCESS_YIELD();

        if (etimer_expired(&et)) {
            {
                int salt = rand_fixed() % 200;
                static char salts[4];
                int size = snprintf(salts, sizeof(salts), "%d", salt);
                ASSERT(size < sizeof(salts));

                coap_init_message(request, COAP_TYPE_CON, COAP_PUT, 0);
                coap_set_header_uri_path(request, "random/salt");
                coap_set_payload(request, (uint8_t*)salts, size);

                printf("setting salt: %d\n", salt);
                COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, client_chunk_handler);
            }

            {
                int len = rand_fixed() % 200;
                static char query[4+4];
                int size = snprintf(query, sizeof(query), "len=%d", len);
                ASSERT(size < sizeof(query));

                coap_init_message(request, COAP_TYPE_CON, COAP_GET, 0);
                coap_set_header_uri_path(request, "random");
                coap_set_header_uri_query(request, query);

                printf("query random: %s\n", query);
                COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, client_chunk_handler);
            }

            etimer_reset(&et);
        }
    }

    PROCESS_END();
}
