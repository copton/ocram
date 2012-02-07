#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "contiki.h"
#include "contiki-net.h"

#include "erbium.h"
#include "er-coap-07.h"

static char rands[] = {"123456789abcdefghikjlmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZY"};

static int nextRand(int y)
{
    return (8 * y + 5) % (sizeof(rands) - 1 /* trailing 0 byte */);
}

static void genRand(int salt, uint8_t* buffer, uint16_t skip, uint16_t count)
{
    int state = salt;
    int i;
    for (i=0; i<skip+count; i++) {
        state = nextRand(state);
        if (i >= skip) {
            buffer[i-skip] = rands[state];
        }
    }    
}

int salt = 0;

RESOURCE(salt, METHOD_POST, "random/salt", "title\"Salt of Random Generator, POST salt=1..\";rt=\"Data\"");

void salt_handler(void* request, void* response, uint8_t* buffer, uint16_t preferred_size, int32_t* offset)
{
    printf("salt_handler\n");
    const rest_resource_flags_t method = REST.get_method_type(request);
    if (method == METHOD_POST) {
        const char* salts;
        coap_get_payload(request, (const uint8_t**)&salts);
        int new_salt = atoi(salts);
        if (new_salt == 0) {
            REST.set_response_status(response, REST.status.BAD_REQUEST);
            const char error[] = "payload must be a non-zero, positive integer.";
            REST.set_response_payload(response, (uint8_t*)error, sizeof(error));
        } else {
            salt = new_salt - 1;
            printf("received new salt: %d\n", new_salt);
            REST.set_response_status(response, REST.status.CHANGED);
        }
    } else {
        REST.set_response_status(response, REST.status.METHOD_NOT_ALLOWED);
    }
}

RESOURCE(random, METHOD_GET, "random", "title\"Random Generator :?len=1..\";rt=\"Data\"");

void random_handler(void* request, void* response, uint8_t* buffer, uint16_t preferred_size, int32_t* offset)
{
    printf("random_handler\n");
    const rest_resource_flags_t method = REST.get_method_type(request);
    if (method == METHOD_GET) {
        const char* lengths = NULL;
        if (REST.get_query_variable(request, "len", &lengths)) {
            int length = atoi(lengths);
            if (length <= 0) {
                REST.set_response_status(response, REST.status.BAD_REQUEST);
                const char error[] = "query variable 'len' has to be a non-zero, positive integer";
                REST.set_response_payload(response, (uint8_t*)error, sizeof(error));
            } else if (length < *offset) {
                REST.set_response_status(response, REST.status.BAD_REQUEST);
                const char error[] = "internal error";
                REST.set_response_payload(response, (uint8_t*)error, sizeof(error));
            } else {
                int skip = *offset;
                length -= skip;
                int count;
                if (length > preferred_size) {
                    count = preferred_size;
                    *offset += preferred_size;
                } else {
                    count = length; 
                    *offset = -1;
                }
                genRand(salt, buffer, skip, count);
                REST.set_header_content_type(response, REST.type.TEXT_PLAIN);
//            REST.set_header_etag(response, (uint8_t *) &length, 1);
                REST.set_response_payload(response, buffer, length);
            }
        } else {
            REST.set_response_status(response, REST.status.BAD_REQUEST);
            const char error[] = "query variable 'len' is required for GET queries";
            REST.set_response_payload(response, (uint8_t*)error, sizeof(error));
        }
    } else {
        REST.set_response_status(response, REST.status.METHOD_NOT_ALLOWED);
    }
}

PROCESS(rest_server, "Rest Server");
AUTOSTART_PROCESSES(&rest_server);

PROCESS_THREAD(rest_server, ev, data)
{
  PROCESS_BEGIN();

  printf("Rest Server\n");
  printf("REST max chunk: %u\n", REST_MAX_CHUNK_SIZE);

  /* Initialize the REST framework. */
  rest_init_framework();

  rest_activate_resource(&resource_random);

  while(1) {
    PROCESS_YIELD();
  }

  PROCESS_END();
}
