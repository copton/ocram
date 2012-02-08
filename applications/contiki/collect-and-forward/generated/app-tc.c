#include "os/tc_receive.h"
#include "os/tc_send.h"
#include "os/tc_sleep.h"
#include "config.h"

#include "dev/light-sensor.h"
#include "net/uiplib.h"
#include "net/uip-debug.h"

#define UIP_IP_BUF   ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#define MAX_NUMBEROF_VALUES 100
uint32_t values[MAX_NUMBEROF_VALUES];
size_t offset;

void collect_run(clock_time_t dt)
{
    SENSORS_ACTIVATE(light_sensor);

    clock_time_t now = clock_time();
    while (true) {
        tc_sleep(now + dt);
        now += dt;

        uint16_t value = light_sensor.value(LIGHT_SENSOR_PHOTOSYNTHETIC);
        printf("reading value from sensor: %u\n", value);
        if (offset == MAX_NUMBEROF_VALUES) {
            printf("collect: overflow! Too many values\n");
        } else {
            values[offset++] = value;
        }
    }
}

void receive_run(uint16_t lport, uint16_t rport)
{
    struct uip_udp_conn *server_conn;
    server_conn = udp_new(NULL, rport, NULL);
    udp_bind(server_conn, lport);

    while (true) {
        uint8_t buffer[10];
        size_t len;

		tc_receive(buffer, sizeof(buffer), &len);

        size_t numberof_values = len / sizeof(uint32_t);
        uint32_t value;

        uip_debug_ipaddr_print(&UIP_IP_BUF->srcipaddr);
        printf(": received values: ");

        size_t i;
        for (i=0; i<numberof_values; i++) {
            if (offset == MAX_NUMBEROF_VALUES) {
                printf("\nreceive: overflow! Too many values");
                break; 
            }
            memcpy(&value, &buffer[i * sizeof(uint32_t)], sizeof(uint32_t));
            printf("%lu ", value);
            values[offset++] = value;
        }

        printf("\n");
    }
}

void send_run(uip_ipaddr_t* ipaddr, uint16_t lport, uint16_t rport, clock_time_t dt)
{
    struct uip_udp_conn* client_conn;

    client_conn = udp_new(NULL, rport, NULL);
    udp_bind(client_conn, lport);

    clock_time_t now = clock_time();

    while (true) {
        tc_sleep(now + dt);
        now += dt;

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

        uip_debug_ipaddr_print(ipaddr);
        printf(": send values: %lu, %lu\n", buf[0], buf[1]);
        tc_send(client_conn, ipaddr, rport, (uint8_t*)&buf[0], sizeof(buf));
    }
}

TC_RUN_THREAD void task_send()
{
    uip_ipaddr_t ipaddr;
    uip_ip6addr(&ipaddr, 0xaaaa, 0, 0, 0, 0, 0x00ff, 0xfe00, 3);
    uip_ds6_addr_add(&ipaddr, 0, ADDR_MANUAL);
	send_run(&ipaddr, UIP_HTONS(UDP_CLIENT_PORT), UIP_HTONS(UDP_SERVER_PORT), 120 * CLOCK_SECOND);
}

TC_RUN_THREAD void task_receive()
{
	receive_run(UIP_HTONS(UDP_SERVER_PORT), UIP_HTONS(UDP_CLIENT_PORT));
}

TC_RUN_THREAD void task_collect()
{
	collect_run(10 * CLOCK_SECOND);
}
