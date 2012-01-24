#include "contiki.h"
#include "contiki-net.h"
#include "dev/light-sensor.h"
#include "net/netstack.h"
#include "net/uip.h"

#include "common.h"
#include "../../../os/types.h"

// config
#define DT_SEND (120 * CLOCK_SECOND)
#define DT_COLLECT (10 * CLOCK_SECOND)

// globals
struct uip_udp_conn *server_conn, *client_conn;
uip_ipaddr_t server_ipaddr;

#define MAX_NUMBEROF_VALUES 100
uint32_t values[MAX_NUMBEROF_VALUES];
size_t offset;

// receive
#define MAX_PAYLOAD_LEN		30
#define UIP_IP_BUF   ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])

PROCESS(receive_process, "receive process");

static void tcpip_handler(void) {
    char* appdata = (char*) uip_appdata;
    size_t numberof_values = uip_datalen() / sizeof(uint32_t);
    uint32_t value;
    size_t i;

    if(! uip_newdata()) {
        printf("tcpip handler called for no reason(?)\n");
        return;
    }

    if ((uip_datalen() % sizeof(uint32_t)) != 0) {
        printf("ignoring packet of unexpected size\n");
        return;
    }

    PRINT6ADDR(&UIP_IP_BUF->srcipaddr);
    printf(": received values: ");
    for (i=0; i<numberof_values; i++) {
        if (offset == MAX_NUMBEROF_VALUES) {
            printf("\noverflow! Too many values\n");
            return;
        }
        memcpy(&value, &appdata[i * sizeof(uint32_t)], sizeof(uint32_t));
        printf("%lu ", value);
        values[offset++] = value;
    }
    printf("\n");
}

PROCESS_THREAD(receive_process, ev, data)
{
    PROCESS_BEGIN();
    
    // udp server connection
    server_conn = udp_new(NULL, UIP_HTONS(UDP_CLIENT_PORT), NULL);
    udp_bind(server_conn, UIP_HTONS(UDP_SERVER_PORT));

    PRINTF("app/receive started\n");

    while(1) {
        PROCESS_YIELD();
        if(ev == tcpip_event) {
            tcpip_handler();
        }
    }

    PROCESS_END();
}

// send
PROCESS(send_process, "send process");

static void send_packet() {
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

    PRINT6ADDR(&server_ipaddr);
    printf(": send values: %lu, %lu\n", buf[0], buf[1]);
    uip_udp_packet_sendto(client_conn, &buf[0], sizeof(buf), &server_ipaddr, UIP_HTONS(UDP_SERVER_PORT));
}

PROCESS_THREAD(send_process, ev, data)
{
    static struct etimer periodic;

    PROCESS_BEGIN();
    
    // server address
    uip_ip6addr(&server_ipaddr, 0xfe80, 0, 0, 0, 0x212, 0x7403, 0x3, 0x303);
    // udp client connection
    client_conn = udp_new(NULL, UIP_HTONS(UDP_SERVER_PORT), NULL); 
    udp_bind(client_conn, UIP_HTONS(UDP_CLIENT_PORT)); 

    PRINTF("app/send started\n");

    etimer_set(&periodic, DT_SEND);
    while(1) {
        PROCESS_YIELD();
        if (etimer_expired(&periodic)) {
            etimer_reset(&periodic);
            send_packet();
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

    PRINTF("app/collect started\n");

    etimer_set(&periodic, DT_COLLECT);

    while(1) {
        PROCESS_YIELD();
        if(etimer_expired(&periodic)) {
            etimer_reset(&periodic);
            uint16_t value = light_sensor.value(LIGHT_SENSOR_PHOTOSYNTHETIC);
            printf("reading value from sensor: %u\n", value);
            if (offset == MAX_NUMBEROF_VALUES) {
                printf("collect: overflow! Too many values\n");
            } else {
                values[offset++] = value;
            }
        }
    }

    PROCESS_END();
}

// config
PROCESS(config_process, "config process");

PROCESS_THREAD(config_process, ev, data)
{
    PROCESS_BEGIN();
    PROCESS_PAUSE();

    // local IP address
    uip_ipaddr_t ipaddr;
    uip_ip6addr(&ipaddr, 0xaaaa, 0, 0, 0, 0, 0x00ff, 0xfe00, 3);
    uip_ds6_addr_add(&ipaddr, 0, ADDR_MANUAL);
    // no duty-cycling
    NETSTACK_MAC.off(1);
    print_local_addresses();

    offset = 0;

    PROCESS_PAUSE();

    process_start(&receive_process, NULL);
    process_start(&collect_process, NULL);
    process_start(&send_process, NULL);

    PROCESS_END();
}

AUTOSTART_PROCESSES(&config_process);
