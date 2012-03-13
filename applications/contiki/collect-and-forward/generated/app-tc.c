#include <stdlib.h>
#include "tc/tc_receive.h"
#include "tc/tc_send.h"
#include "tc/tc_sleep.h"

#include "sim_assert.h"

#include "contiki.h"
#include "contiki-net.h"
#include "dev/light-sensor.h"
#include "net/netstack.h"
#include "net/uip.h"
#include "net/uip-debug.h"
#include "config.h"

// FIFO
uint32_t values[MAX_NUMBEROF_VALUES];
size_t offset;

void init()
{
    uip_ipaddr_t ipaddr;
    uip_ip6addr(&ipaddr, 0xaaaa, 0, 0, 0, 0, 0x00ff, 0xfe00, 3);
    uip_ds6_addr_add(&ipaddr, 0, ADDR_MANUAL);
    // no duty-cycling
    NETSTACK_MAC.off(1);

    offset = 0;
}

// send
TC_RUN_THREAD void task_send()
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
        tc_sleep(now);

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
        tc_send(client_conn, &server_ipaddr, UIP_HTONS(UDP_SERVER_PORT), (uint8_t*)&buf[0], sizeof(buf));
    }
}

// receive
#define UIP_IP_BUF   ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])
TC_RUN_THREAD void receive_run(uint16_t lport, uint16_t rport)
{
    struct uip_udp_conn *server_conn;
    server_conn = udp_new(NULL, UIP_HTONS(UDP_CLIENT_PORT), NULL);
    udp_bind(server_conn, UIP_HTONS(UDP_SERVER_PORT));

    while (1) {
		tc_receive();
        uint8_t* buffer = uip_appdata;

        ASSERT((uip_datalen() % sizeof(uint32_t)) == 0);

        size_t numberof_values = uip_datalen() / sizeof(uint32_t);
        uint32_t value;

        printf("trace: received values: ");

        size_t i;
        for (i=0; i<numberof_values; i++) {
            ASSERT(offset < MAX_NUMBEROF_VALUES);
            memcpy(&value, &buffer[i * sizeof(uint32_t)], sizeof(uint32_t));
            printf("%lu ", value);
            values[offset++] = value;
        }
        printf("\n");
    }
}

// collect
TC_RUN_THREAD void collect_run()
{
    SENSORS_ACTIVATE(light_sensor);

    clock_time_t now = clock_time();
    while (1) {
        now += DT_COLLECT;
        tc_sleep(now);

        uint16_t value = light_sensor.value(LIGHT_SENSOR_PHOTOSYNTHETIC);
        value = rand(); // enable comparison of logs
        printf("trace: reading value from sensor: %u\n", value);
        ASSERT(offset < MAX_NUMBEROF_VALUES);
        values[offset++] = value;
    }
}
