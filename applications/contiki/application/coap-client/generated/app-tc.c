#include "../../../os/tc.h"
#include <stdbool.h>

#include "dev/button-sensor.h"
#include "er-coap-07-engine.h"

#define TOGGLE_INTERVAL (10 * CLOCK_SECOND)

#define REMOTE_PORT     UIP_HTONS(COAP_DEFAULT_PORT)

#define NUMBER_OF_URLS 4
char* service_urls[NUMBER_OF_URLS] = {".well-known/core", "/actuators/toggle", "battery/", "error/in//path"};
static int uri_switch = 0;

void send_coap_request(coap_method_t method, const char* url, uint8_t* payload, size_t len)
{
    coap_packet_t request[1];
    uip_ipaddr_t server_ipaddr;
    uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x0212, 0x7402, 0x0002, 0x0202);

    coap_init_message(request, COAP_TYPE_CON, method, 0);
    coap_set_header_uri_path(request, (char*)url);
    if (payload) {
        coap_set_payload(request, payload, len);
    }

    // TODO: COAP_BLOCKING_REQUEST
}

TC_RUN_THREAD void task_toggle()
{
    static char toggleCommand[] = "Toggle!";
    clock_time_t timestamp = clock_time();
    while(true) {
        timestamp += TOGGLE_INTERVAL;
        tc_sleep(timestamp);
        send_coap_request(COAP_POST, service_urls[1], toggleCommand, sizeof(toggleCommand));
    }
}

TC_RUN_THREAD void task_react()
{
    while(true) {
        SENSORS_ACTIVATE(button_sensor);
        tc_await_button();
        send_coap_request(COAP_GET, service_urls[uri_switch], 0, 0);
        uri_switch = (uri_switch + 1) % NUMBER_OF_URLS;
    }
}

