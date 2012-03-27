#include <stdlib.h>
#include "contiki.h"
#include "contiki-net.h"
#include "dev/light-sensor.h"
#include "net/netstack.h"
#include "net/uip.h"
#include "net/uip-debug.h"

#include "common.h"
#include "config.h"
#include "cooja.h"

uint32_t values[MAX_NUMBEROF_VALUES];
size_t offset;

void init()
{
    ipconfig(false);
    offset = 0;
}

// send
PROCESS(send_process, "send process");

PROCESS_THREAD(send_process, ev, data)
{
    static struct etimer periodic;
    static uip_ipaddr_t server_ipaddr;
    static struct uip_udp_conn* client_conn;

    PROCESS_BEGIN();

    init();
    
    uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x212, 0x7403, 0x3, 0x303);
    client_conn = udp_new(NULL, UIP_HTONS(UDP_SERVER_PORT), NULL); 
    udp_bind(client_conn, UIP_HTONS(UDP_CLIENT_PORT)); 

    etimer_set(&periodic, DT_SEND);
    while(1) {
        PROCESS_YIELD();
        if (etimer_expired(&periodic)) {
            etimer_reset(&periodic);
            uint32_t buf[2] = {0xFFFFFFFF, 0};
            size_t i;

            for (i = 0; i<offset; i++) {
                if (values[i] < buf[0]) {
                    buf[0] = values[i];
                }
                if (values[i] > buf[1]) {
                    buf[1] = values[i];
                }
            }
            offset = 0;

            printf("trace: send values: %lu %lu\n", buf[0], buf[1]);
            uip_udp_packet_sendto(client_conn, &buf[0], sizeof(buf), &server_ipaddr, UIP_HTONS(UDP_SERVER_PORT));
        }
    }

    PROCESS_END();
}


// receive
#define UIP_IP_BUF   ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])
PROCESS(receive_process, "receive process");
PROCESS_THREAD(receive_process, ev, data)
{
    PROCESS_BEGIN();
    
    static struct uip_udp_conn* server_conn;
    server_conn = udp_new(NULL, UIP_HTONS(UDP_CLIENT_PORT), NULL);
    udp_bind(server_conn, UIP_HTONS(UDP_SERVER_PORT));

    while(1) {
        PROCESS_YIELD();
        if(ev == tcpip_event) {
            uint8_t* appdata = uip_appdata;
            ASSERT((uip_datalen() % sizeof(uint32_t)) == 0);
            size_t numberof_values = uip_datalen() / sizeof(uint32_t);


            printf("trace: received values: ");
            uint32_t value;
            size_t i;
            for (i=0; i<numberof_values; i++) {
                ASSERT(offset < MAX_NUMBEROF_VALUES);
                memcpy(&value, &appdata[i * sizeof(uint32_t)], sizeof(uint32_t));
                printf("%lu ", value);
                values[offset++] = value;
            }
            printf("\n");
        }
    }

    PROCESS_END();
}

// collect
PROCESS(collect_process, "collect process");
PROCESS_THREAD(collect_process, ev, data)
{
    static struct etimer periodic;

    PROCESS_BEGIN();

    SENSORS_ACTIVATE(light_sensor);

    etimer_set(&periodic, DT_COLLECT);

    while(1) {
        PROCESS_YIELD();
        if(etimer_expired(&periodic)) {
            etimer_reset(&periodic);
            uint16_t value = light_sensor.value(LIGHT_SENSOR_PHOTOSYNTHETIC);
            value = rand(); // enable comparison of logs
            printf("trace: reading value from sensor: %u\n", value);
            ASSERT(offset < MAX_NUMBEROF_VALUES);
            values[offset++] = value;
        }
    }

    PROCESS_END();
}

AUTOSTART_PROCESSES(&send_process, &receive_process, &collect_process);
