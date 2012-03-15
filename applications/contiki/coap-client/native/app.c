#include <stdio.h>
#include <stdlib.h>
#include "contiki.h"
#include "coap.h"
#include "debug.h"

// config
#define TOGGLE_INTERVAL (10 * CLOCK_SECOND)
#define REMOTE_PORT     UIP_HTONS(COAP_DEFAULT_PORT)

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
static struct process *transaction_handler_process = NULL;

extern list_t transactions_list;
extern struct memb transactions_memb;

#define COAP_RESPONSE_TIMEOUT_TICKS         (CLOCK_SECOND * COAP_RESPONSE_TIMEOUT)
#define COAP_RESPONSE_TIMEOUT_BACKOFF_MASK  ((CLOCK_SECOND * COAP_RESPONSE_TIMEOUT * (COAP_RESPONSE_RANDOM_FACTOR - 1)) + 1.5)

void coap_register_as_transaction_handler()
{
  transaction_handler_process = PROCESS_CURRENT();
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

      /*FIXME hack, maybe there is a better way, but avoid posting everything to the process */
      struct process *process_actual = PROCESS_CURRENT();
      process_current = transaction_handler_process;
      etimer_restart(&t->retrans_timer); /* interval updated above */
      process_current = process_actual;

      list_add(transactions_list, t); /* List itself makes sure same element is not added twice. */

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
struct request_state_t {
    struct pt pt;
    struct process *process;
    coap_transaction_t *transaction;
    coap_packet_t *response;
    uint32_t block_num;
};

typedef void (*blocking_response_handler) (void* response);

PT_THREAD(coap_blocking_request(struct request_state_t *state, process_event_t ev,
                                uip_ipaddr_t *remote_ipaddr, uint16_t remote_port,
                                coap_packet_t *request,
                                blocking_response_handler request_callback));

#define COAP_BLOCKING_REQUEST(server_addr, server_port, request, chunk_handler) \
static struct request_state_t request_state; \
PT_SPAWN(process_pt, &request_state.pt, \
             coap_blocking_request(&request_state, ev, \
                                   server_addr, server_port, \
                                   request, chunk_handler) \
    );
void blocking_request_callback(void *callback_data, void *response) {
  struct request_state_t *state = (struct request_state_t *) callback_data;
  state->response = (coap_packet_t*) response;
  process_poll(state->process);
}

PT_THREAD(coap_blocking_request(struct request_state_t *state, process_event_t ev,
                                uip_ipaddr_t *remote_ipaddr, uint16_t remote_port,
                                coap_packet_t *request,
                                blocking_response_handler request_callback)) {
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
      state->transaction->callback = blocking_request_callback;
      state->transaction->callback_data = state;

      if (state->block_num>0)
      {
        coap_set_header_block2(request, state->block_num, 0, REST_MAX_CHUNK_SIZE);
      }

      state->transaction->packet_len = coap_serialize_message(request, state->transaction->packet);

      coap_send_transaction(state->transaction);

      PT_YIELD_UNTIL(&state->pt, ev == PROCESS_EVENT_POLL);

      if (!state->response)
      {
        PT_EXIT(&state->pt);
      }

      coap_get_header_block2(state->response, &res_block, &more, NULL, NULL);

      if (res_block==state->block_num)
      {
        request_callback(state->response);
        ++(state->block_num);
      }
      else
      {
        ++block_error;
      }
    }
    else
    {
      PT_EXIT(&state->pt);
    }
  } while (more && block_error<COAP_MAX_ATTEMPTS);

  PT_END(&state->pt);
}


// RECEIVER
PROCESS(coap_receiver, "CoAP Receiver");

void coap_receiver_init()
{
  process_start(&coap_receiver, NULL);
}

PROCESS_THREAD(coap_receiver, ev, data)
{
  PROCESS_BEGIN();

  // rest_activate_resource(&resource_well_known_core); {# clients don't have resources #}

  coap_register_as_transaction_handler();
  coap_init_connection(SERVER_LISTEN_PORT);

  while(1) {
    PROCESS_YIELD();

    if(ev == tcpip_event) {
      handle_incoming_data();
    } else if (ev == PROCESS_EVENT_TIMER) {
      /* retransmissions are handled here */
      coap_check_transactions();
    }
  } /* while (1) */

  PROCESS_END();
}

// MAIN
void client_chunk_handler(void *response)
{
    coap_packet_t* packet = response;
    const uint8_t *chunk;
    int len = coap_get_payload(response, &chunk);
    printf("trace: coap response: %d.%02d: %.*s\n", packet->code/32, packet->code % 32, len, (char *)chunk);
}

PROCESS(coap_client, "COAP Client");
PROCESS_THREAD(coap_client, ev, data)
{
    static uip_ipaddr_t server_ipaddr;
    static coap_packet_t request[1];
    static struct etimer et;

    PROCESS_BEGIN();

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

                printf("trace: setting salt: %d\n", salt);
                COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, &client_chunk_handler);
            }

            {
                int len = rand_fixed() % 200;
                static char query[4+4];
                int size = snprintf(query, sizeof(query), "len=%d", len);
                ASSERT(size < sizeof(query));

                coap_init_message(request, COAP_TYPE_CON, COAP_GET, 0);
                coap_set_header_uri_path(request, "random");
                coap_set_header_uri_query(request, query);

                printf("trace: query random: %s\n", query);
                COAP_BLOCKING_REQUEST(&server_ipaddr, REMOTE_PORT, request, &client_chunk_handler);
            }

            etimer_reset(&et);
        }
    }

    PROCESS_END();
}

AUTOSTART_PROCESSES(&coap_client);
