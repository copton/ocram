#include "contiki.h"
#include <string.h>
#include "dev/serial-line.h"

PROCESS_NAME(echo_server);
AUTOSTART_PROCESSES(&echo_server);

PROCESS(echo_server, "echo server");

PROCESS_THREAD(echo_server, ev, data)
{
	PROCESS_BEGIN();

	printf ("hallo\n");
	static struct etimer et;
	static char buffer[512];
	while(1) {
		PROCESS_WAIT_EVENT_UNTIL(ev == serial_line_event_message && data != NULL);
		strcpy(buffer, data);
		printf("XXX %s\n", buffer);
		etimer_set(&et, CLOCK_SECOND);
		PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&et));
		printf("XXX %s\n", buffer);
	}

	PROCESS_END();
}
