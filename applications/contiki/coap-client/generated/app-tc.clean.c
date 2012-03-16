#include <stdio.h>
#include <stdlib.h>

#include "tc/tc_sleep.h"
#include "tc/tc_receive.h"
#include "tc/tc_condition.h"

#include "coap.h"
#include "debug.h"

// config
#define TOGGLE_INTERVAL (10 * CLOCK_SECOND)
#define REMOTE_PORT     UIP_HTONS(COAP_DEFAULT_PORT)
void client_chunk_handler(void *response);

// rand is not standard compliant on this platform and sometimes returns negative values
int rand_fixed()
{
    int value;
    do {
        value = rand();
    } while (value < 0);
    return value;
}

// TRANSACTION

extern list_t transactions_list;
extern struct memb transactions_memb;

#define COAP_RESPONSE_TIMEOUT_TICKS         (CLOCK_SECOND * COAP_RESPONSE_TIMEOUT)
#define COAP_RESPONSE_TIMEOUT_BACKOFF_MASK  ((CLOCK_SECOND * COAP_RESPONSE_TIMEOUT * (COAP_RESPONSE_RANDOM_FACTOR - 1)) + 1.5)

condition_t transactions_cond = TC_CONDITION_INITIALIZER;
coap_transaction_t* new_transaction = NULL;

TC_RUN_THREAD void task_transactions()
{
    while(1) {
        if (new_transaction) {
           etimer_restart(&new_transaction->retrans_timer); 
           new_transaction = NULL;
        }
        if (tc_condition_time_wait(&transactions_cond)) {
            coap_check_transactions();
        }
    }
}

void coap_send_transaction(coap_transaction_t *t)
{
  coap_send_message(&t->addr, t->port, t->packet, t->packet_len);

  if (COAP_TYPE_CON==((COAP_HEADER_TYPE_MASK & t->packet[0])>>COAP_HEADER_TYPE_POSITION))
  {
    if (t->retrans_counter<COAP_MAX_RETRANSMIT)
    {
      if (t->retrans_counter==0)
      {
        t->retrans_timer.timer.interval = COAP_RESPONSE_TIMEOUT_TICKS + (random_rand() % (clock_time_t) COAP_RESPONSE_TIMEOUT_BACKOFF_MASK);
      }
      else
      {
        t->retrans_timer.timer.interval <<= 1; /* double */
      }

      list_add(transactions_list, t);
      new_transaction = t;
      tc_condition_signal(&transactions_cond);

      t = NULL;
    }
    else
    {
      /* timeout */
      restful_response_handler callback = t->callback;
      void *callback_data = t->callback_data;

      /* handle observers */
      coap_remove_observer_by_client(&t->addr, t->port);

      coap_clear_transaction(t);

      if (callback) {
        callback(callback_data, NULL);
      }
    }
  }
  else
  {
    coap_clear_transaction(t);
  }
}

// BLOCKWISE TRANSFER
typedef struct {
    coap_packet_t* response;
    condition_t cond;
} blocking_ctx_t;

void blocking_request_callback(void* data, void* response)
{
    blocking_ctx_t* ctx = data;
    ctx->response = response;
    tc_condition_signal(&ctx->cond);
}

void coap_request(uip_ipaddr_t *remote_ipaddr, uint16_t remote_port, coap_packet_t *request)
{
    uint8_t more = 0;
    uint32_t res_block = 0;
    uint8_t block_error = 0;
    uint32_t block_num = 0;
    coap_transaction_t* transaction;
    blocking_ctx_t ctx;
    tc_condition_init(&ctx.cond);

    do {
        request->tid = coap_get_tid();
        if ((transaction = coap_new_transaction(request->tid, remote_ipaddr, remote_port)))
        {
            transaction->callback = blocking_request_callback;
            transaction->callback_data = &ctx;

            if (block_num>0)
            {
                coap_set_header_block2(request, block_num, 0, REST_MAX_CHUNK_SIZE);
            }

            transaction->packet_len = coap_serialize_message(request, transaction->packet);

            coap_send_transaction(transaction);

            tc_condition_wait(&ctx.cond);

            if (!ctx.response)
            {
                return;
            }

            coap_get_header_block2(ctx.response, &res_block, &more, NULL, NULL);

            if (res_block == block_num)
            {
                client_chunk_handler(ctx.response);
                ++(block_num);
            }
            else
            {
                ++block_error;
            }
        }
        else
        {
            return;
        }
    } while (more && block_error<COAP_MAX_ATTEMPTS);
}

// RECEIVER
condition_t start_receive_thread = TC_CONDITION_INITIALIZER;

void coap_receiver_init()
{
    tc_condition_signal(&start_receive_thread);
}

TC_RUN_THREAD void task_receive()
{
    tc_condition_wait(&start_receive_thread);
    coap_init_connection(SERVER_LISTEN_PORT);
    while(1) {
        tc_receive();
        handle_incoming_data(); 
    }
}

// MAIN
void client_chunk_handler(void *response)
{
    coap_packet_t* packet = response;
    const uint8_t *chunk;
    int len = coap_get_payload(response, &chunk);
    printf("trace: coap response: %d.%02d: %.*s\n", packet->code/32, packet->code % 32, len, (char *)chunk);
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

            printf("trace: setting salt: %d\n", salt);
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

            printf("trace: query random: %s\n", query);
            coap_request(&server_ipaddr, REMOTE_PORT, request);
        }
    }
}
