#include <stdio.h>
#include <stdlib.h>

#include "os/tc_sleep.h"
#include "os/tc_coap_send_transaction.h"

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

void coap_request(uip_ipaddr_t *remote_ipaddr, uint16_t remote_port, coap_packet_t *request)
{
    uint8_t more = 0;
    uint32_t res_block = 0;
    uint8_t block_error = 0;
    uint32_t block_num = 0;
    coap_packet_t* response = NULL;
    coap_transaction_t* transaction;

    do {
        request->tid = coap_get_tid();
        if ((transaction = coap_new_transaction(request->tid, remote_ipaddr, remote_port)))
        {
            if (block_num>0)
            {
                coap_set_header_block2(request, block_num, 0, REST_MAX_CHUNK_SIZE);
            }

            transaction->packet_len = coap_serialize_message(request, transaction->packet);

            response = tc_coap_send_transaction(transaction);
            printf("Requested #%lu (TID %u)\n", block_num, request->tid);

            if (!response)
            {
                printf("Server not responding\n");
                return;
            }

            coap_get_header_block2(response, &res_block, &more, NULL, NULL);

            printf("Received #%lu%s (%u bytes)\n", res_block, more ? "+" : "", response->payload_len);

            if (res_block == block_num)
            {
                client_chunk_handler(response);
                ++(block_num);
            }
            else
            {
                printf("WRONG BLOCK %lu/%lu\n", res_block, block_num);
                ++block_error;
            }
        }
        else
        {
            printf("Could not allocate transaction buffer");
            return;
        }
    } while (more && block_error<COAP_MAX_ATTEMPTS);
}

TC_RUN_THREAD void task_query()
{
    coap_receiver_init();

    uip_ipaddr_t server_ipaddr;
    uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202);

    coap_packet_t request[1];

    clock_time_t timestamp = clock_time();
    while(1) {
        timestamp += TOGGLE_INTERVAL;
        tc_sleep(timestamp);

        {
            int salt = rand_fixed() % 200;
            char salts[4];
            int size = snprintf(salts, sizeof(salts), "%d", salt);
            ASSERT(size < sizeof(salts));

            coap_init_message(request, COAP_TYPE_CON, COAP_PUT, 0);
            coap_set_header_uri_path(request, "random/salt");
            coap_set_payload(request, (uint8_t*)salts, size);

            printf("setting salt: %d\n", salt);
            coap_request(&server_ipaddr, REMOTE_PORT, request);
        }

        {
            int len = rand_fixed() % 200;
            char query[4+4];
            int size = snprintf(query, sizeof(query), "len=%d", len);
            ASSERT(size < sizeof(query));

            coap_init_message(request, COAP_TYPE_CON, COAP_GET, 0);
            coap_set_header_uri_path(request, "random");
            coap_set_header_uri_query(request, query);

            printf("query random: %s\n", query);
            coap_request(&server_ipaddr, REMOTE_PORT, request);
        }
    }
}
