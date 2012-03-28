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

#include "tl/tl.h"

uint32_t values[MAX_NUMBEROF_VALUES];
size_t offset;

void init()
{
    ipconfig(false);
    offset = 0;
}

// send
uint8_t stack_send[200];
void task_send()
{
    uip_ipaddr_t server_ipaddr;
    struct uip_udp_conn* client_conn;

    init();

    uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x212, 0x7403, 0x3, 0x303);
    client_conn = udp_new(NULL, UIP_HTONS(UDP_SERVER_PORT), NULL);
    udp_bind(client_conn, UIP_HTONS(UDP_CLIENT_PORT));

    clock_time_t now = clock_time();

    while (1) {
        now += DT_SEND;
        tl_sleep(now, NULL);

        uint32_t buf[2];
        buf[0] = 0xFFFFFFFF;
        buf[1] = 0;
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
        uip_udp_packet_sendto(client_conn, (uint8_t*)&buf[0], sizeof(buf), &server_ipaddr, UIP_HTONS(UDP_SERVER_PORT));
    }
}

// receive
#define UIP_IP_BUF   ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])
uint8_t stack_receive[154];
void task_receive(uint16_t lport, uint16_t rport)
{
    struct uip_udp_conn *server_conn;
    server_conn = udp_new(NULL, UIP_HTONS(UDP_CLIENT_PORT), NULL);
    udp_bind(server_conn, UIP_HTONS(UDP_SERVER_PORT));

    while (1) {
		tl_receive(server_conn, NULL);
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

// collect
uint8_t stack_collect[162];
void task_collect()
{
    SENSORS_ACTIVATE(light_sensor);

    clock_time_t now = clock_time();
    while (1) {
        now += DT_COLLECT;
        tl_sleep(now, NULL);

        uint16_t value = light_sensor.value(LIGHT_SENSOR_PHOTOSYNTHETIC);
        value = rand(); // enable comparison of logs
        printf("trace: reading value from sensor: %u\n", value);
        ASSERT(offset < MAX_NUMBEROF_VALUES);
        values[offset++] = value;
    }
}

void tl_app_main()
{
    tl_create_thread(&task_send, stack_send, sizeof(stack_send));
    tl_create_thread(&task_receive, stack_receive, sizeof(stack_receive));
    tl_create_thread(&task_collect, stack_collect, sizeof(stack_collect));
}
