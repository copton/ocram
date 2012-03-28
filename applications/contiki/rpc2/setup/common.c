#include "common.h"
#include "contiki.h"
#include "contiki-net.h"
#include "net/rpl/rpl.h"
#include "cooja.h"

#define DEBUG DEBUG_PRINT
#include "net/uip-debug.h"

#include <stdio.h>

void print_local_addresses(void) {
    int i;
    uint8_t state;

    PRINTF("Client IPv6 addresses: ");
    for(i = 0; i < UIP_DS6_ADDR_NB; i++) {
        state = uip_ds6_if.addr_list[i].state;
        if(uip_ds6_if.addr_list[i].isused && (state == ADDR_TENTATIVE || state == ADDR_PREFERRED)) {
            PRINT6ADDR(&uip_ds6_if.addr_list[i].ipaddr);
            PRINTF(" ");
            /* hack to make address "final" */
            if (state == ADDR_TENTATIVE) {
                uip_ds6_if.addr_list[i].state = ADDR_PREFERRED;
            }
        }
    }
    PRINTF("\n");
}

void print_buffer(uint8_t* buffer, size_t len)
{
    size_t i;
    printf("(%d) ", len);
    for (i=0; i<len; i++) {
        printf("%.2x", buffer[i]);
    }
}

void ipconfig(bool root)
{
    struct uip_ds6_addr *root_if;
    uip_ipaddr_t ipaddr;

    // local address
    uip_ip6addr(&ipaddr, 0xaaaa, 0, 0, 0, 0, 0, 0, 0);
    uip_ds6_addr_add(&ipaddr, 0, ADDR_AUTOCONF);

    if (!root) return;
    
    // RPL stuff...
    root_if = uip_ds6_addr_lookup(&ipaddr);
    ASSERT(root_if != NULL);
    rpl_dag_t *dag;
    dag = rpl_set_root(RPL_DEFAULT_INSTANCE, (uip_ip6addr_t *)&ipaddr);
    uip_ip6addr(&ipaddr, 0xaaaa, 0, 0, 0, 0, 0, 0, 0);
    rpl_set_prefix(dag, &ipaddr, 64);
    PRINTF("created a new RPL dag\n");

    // no duty-cycling
    NETSTACK_MAC.off(1);
    print_local_addresses();
}
