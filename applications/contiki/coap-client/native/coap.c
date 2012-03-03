// {# er-coap-07.c #}
#include "contiki.h"
#include "contiki-net.h"
#include <string.h>
#include <stdio.h>

// {#
// #include "er-coap-07.h"
// #include "er-coap-07-transactions.h"
#include "coap.h"
// #}

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#define PRINTF(...) printf(__VA_ARGS__)
#define PRINT6ADDR(addr) PRINTF("[%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x]", ((u8_t *)addr)[0], ((u8_t *)addr)[1], ((u8_t *)addr)[2], ((u8_t *)addr)[3], ((u8_t *)addr)[4], ((u8_t *)addr)[5], ((u8_t *)addr)[6], ((u8_t *)addr)[7], ((u8_t *)addr)[8], ((u8_t *)addr)[9], ((u8_t *)addr)[10], ((u8_t *)addr)[11], ((u8_t *)addr)[12], ((u8_t *)addr)[13], ((u8_t *)addr)[14], ((u8_t *)addr)[15])
#define PRINTLLADDR(lladdr) PRINTF("[%02x:%02x:%02x:%02x:%02x:%02x]",(lladdr)->addr[0], (lladdr)->addr[1], (lladdr)->addr[2], (lladdr)->addr[3],(lladdr)->addr[4], (lladdr)->addr[5])
#else
#define PRINTF(...)
#define PRINT6ADDR(addr)
#define PRINTLLADDR(addr)
#endif

/*-----------------------------------------------------------------------------------*/
/*- Variables -----------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
static struct uip_udp_conn *udp_conn = NULL;
static uint16_t current_tid = 0;

coap_status_t coap_error_code = NO_ERROR;
char *coap_error_message = "";
/*-----------------------------------------------------------------------------------*/
/*- LOCAL HELP FUNCTIONS ------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
static
uint16_t
log_2(uint16_t value)
{
  uint16_t result = 0;
  do {
    value = value >> 1;
    result++;
  } while (value);

  return result ? result - 1 : result;
}
/*-----------------------------------------------------------------------------------*/
static
uint32_t
parse_int_option(uint8_t *bytes, uint16_t length)
{
  uint32_t var = 0;
  int i = 0;
  while (i<length)
  {
    var <<= 8;
    var |= 0xFF & bytes[i++];
  }
  return var;
}
static
size_t
set_option_header(int delta, size_t length, uint8_t *buffer)
{
  if (length<15)
  {
    buffer[0] = (0x0F & length) | (0xF0 & delta<<4);
    return 1;
  }
  else
  {
    buffer[0] = 0x0F | (0xF0 & delta<<4);
    buffer[1] = 0xFF & (length - 15);
    return 2;
  }
}
/*-----------------------------------------------------------------------------------*/
static
size_t
insert_option_fence_posts(int number, int *current_number, uint8_t *buffer)
{
  size_t i = 0;
  while (number-*current_number > 15)
  {
    uint8_t delta = COAP_OPTION_FENCE_POST - (*current_number%COAP_OPTION_FENCE_POST);
    set_option_header(delta, 0, &buffer[i++]);
    *current_number += delta;

    PRINTF("OPTION FENCE POST delta %u\n", delta);
  }
  return i;
}
/*-----------------------------------------------------------------------------------*/
static
size_t
serialize_int_option(int number, int current_number, uint8_t *buffer, uint32_t value)
{
  /* Insert fence-posts for large deltas */
  size_t i = insert_option_fence_posts(number, &current_number, buffer);
  size_t start_i = i;

  uint8_t *option = &buffer[i];

  if (0xFF000000 & value) buffer[++i] = (uint8_t) (0xFF & value>>24);
  if (0xFFFF0000 & value) buffer[++i] = (uint8_t) (0xFF & value>>16);
  if (0xFFFFFF00 & value) buffer[++i] = (uint8_t) (0xFF & value>>8);
  if (0xFFFFFFFF & value) buffer[++i] = (uint8_t) (0xFF & value);

  i += set_option_header(number - current_number, i-start_i, option);

  PRINTF("OPTION type %u, delta %u, len %u\n", number, number - current_number, i-start_i);

  return i;
}
/*-----------------------------------------------------------------------------------*/
/*
 * Pass the char to split the string at in split_option and receive the number of options in split_option on return.
 */
static
size_t
serialize_array_option(int number, int current_number, uint8_t *buffer, uint8_t *array, size_t length, uint8_t *split_option)
{
  /* Insert fence-posts for large deltas */
  size_t i = insert_option_fence_posts(number, &current_number, buffer);

  if (split_option!=NULL)
  {
    int j;
    uint8_t *part_start = array;
    uint8_t *part_end = NULL;
    size_t temp_length;

    char split_char = *split_option;
    *split_option = 0; /* Ensure reflecting the created option count */

    for (j = 0; j<=length; ++j)
    {
      if (array[j]==split_char || j==length)
      {
        part_end = array + j;
        temp_length = part_end-part_start;

        i += set_option_header(number - current_number, temp_length, &buffer[i]);
        memcpy(&buffer[i], part_start, temp_length);
        i += temp_length;

        PRINTF("OPTION type %u, delta %u, len %u, part [%.*s]\n", number, number - current_number, i, temp_length, part_start);

        ++(*split_option);
        ++j; /* skip the slash */
        current_number = number;
        while( array[j]=='/') ++j;
        part_start = array + j;
      }
    } /* for */
  }
  else
  {
    i += set_option_header(number - current_number, length, &buffer[i]);
    memcpy(&buffer[i], array, length);
    i += length;

    PRINTF("OPTION type %u, delta %u, len %u\n", number, number - current_number, i);
  }

  return i;
}
/*-----------------------------------------------------------------------------------*/
static
void
coap_merge_multi_option(char **dst, size_t *dst_len, uint8_t *option, size_t option_len, char separator)
{
  /* Merge multiple options. */
  if (*dst_len > 0)
  {
    (*dst)[*dst_len] = separator;
    *dst_len += 1;

    /* memmove handles 2-byte option headers */
    memmove((*dst)+(*dst_len), option, option_len);

    *dst_len += option_len;
  }
  else
  {
    *dst = (char *) option; /* thus pointer pointer */
    *dst_len = option_len;
  }
}
/*-----------------------------------------------------------------------------------*/
static
int
coap_get_variable(const char *buffer, size_t length, const char *name, const char **output)
{
  const char *start = NULL;
  const char *end = NULL;
  const char *value_end = NULL;
  size_t name_len = 0;

  /*initialize the output buffer first*/
  *output = 0;

  name_len = strlen(name);
  end = buffer + length;

  for (start = buffer; start + name_len < end; ++start){
    if ((start == buffer || start[-1] == '&') && start[name_len] == '=' &&
        strncmp(name, start, name_len)==0) {

      /* Point start to variable value */
      start += name_len + 1;

      /* Point end to the end of the value */
      value_end = (const char *) memchr(start, '&', end - start);
      if (value_end == NULL) {
        value_end = end;
      }

      *output = start;

      return (value_end - start);
    }
  }

  return 0;
}
/*-----------------------------------------------------------------------------------*/
/*- MEASSAGE SENDING ----------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
void
coap_init_connection(uint16_t port)
{
  /* new connection with remote host */
  udp_conn = udp_new(NULL, 0, NULL);
  udp_bind(udp_conn, port);
  PRINTF("Listening on port %u\n", uip_ntohs(udp_conn->lport));

  /* Initialize transaction ID. */
  current_tid = random_rand();
}
/*-----------------------------------------------------------------------------------*/
uint16_t
coap_get_tid()
{
  return ++current_tid;
}
/*-----------------------------------------------------------------------------------*/
/*- MEASSAGE PROCESSING -------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
void
coap_init_message(void *packet, coap_message_type_t type, uint8_t code, uint16_t tid)
{
  /* Important thing */
  memset(packet, 0, sizeof(coap_packet_t));

  ((coap_packet_t *)packet)->type = type;
  ((coap_packet_t *)packet)->code = code;
  ((coap_packet_t *)packet)->tid = tid;
}
/*-----------------------------------------------------------------------------------*/
size_t
coap_serialize_message(void *packet, uint8_t *buffer)
{
  /* Initialize */
  ((coap_packet_t *)packet)->buffer = buffer;
  ((coap_packet_t *)packet)->version = 1;
  ((coap_packet_t *)packet)->option_count = 0;

  /* serialize options */
  uint8_t *option = ((coap_packet_t *)packet)->buffer + COAP_HEADER_LEN;
  int current_number = 0;

  PRINTF("-Serializing options-\n");

  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_CONTENT_TYPE)) {
    PRINTF("Content-Type [%u]\n", ((coap_packet_t *)packet)->content_type);

    option += serialize_int_option(COAP_OPTION_CONTENT_TYPE, current_number, option, ((coap_packet_t *)packet)->content_type);
    ((coap_packet_t *)packet)->option_count += 1;
    current_number = COAP_OPTION_CONTENT_TYPE;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_MAX_AGE)) {
    PRINTF("Max-Age [%lu]\n", ((coap_packet_t *)packet)->max_age);

    option += serialize_int_option(COAP_OPTION_MAX_AGE, current_number, option, ((coap_packet_t *)packet)->max_age);
    ((coap_packet_t *)packet)->option_count += 1;
    current_number = COAP_OPTION_MAX_AGE;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_PROXY_URI)) {
    PRINTF("Proxy-Uri [%.*s]\n", ((coap_packet_t *)packet)->proxy_uri_len, ((coap_packet_t *)packet)->proxy_uri);

    int length = ((coap_packet_t *)packet)->proxy_uri_len;
    int j = 0;
    while (length>0)
    {
        option += serialize_array_option(COAP_OPTION_PROXY_URI, current_number, option, (uint8_t *) ((coap_packet_t *)packet)->proxy_uri + j*270, MIN(270, length), NULL);
        ((coap_packet_t *)packet)->option_count += 1;
        current_number = COAP_OPTION_PROXY_URI;

        ++j;
        length -= 270;
    }
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_ETAG)) {
    PRINTF("ETag %u [0x%02X%02X%02X%02X%02X%02X%02X%02X]\n", ((coap_packet_t *)packet)->etag_len,
      ((coap_packet_t *)packet)->etag[0],
      ((coap_packet_t *)packet)->etag[1],
      ((coap_packet_t *)packet)->etag[2],
      ((coap_packet_t *)packet)->etag[3],
      ((coap_packet_t *)packet)->etag[4],
      ((coap_packet_t *)packet)->etag[5],
      ((coap_packet_t *)packet)->etag[6],
      ((coap_packet_t *)packet)->etag[7]
    ); /*FIXME always prints 8 bytes */

    option += serialize_array_option(COAP_OPTION_ETAG, current_number, option, ((coap_packet_t *)packet)->etag, ((coap_packet_t *)packet)->etag_len, NULL);
    ((coap_packet_t *)packet)->option_count += 1;
    current_number = COAP_OPTION_ETAG;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_HOST)) {
    PRINTF("Uri-Host [%.*s]\n", ((coap_packet_t *)packet)->uri_host_len, ((coap_packet_t *)packet)->uri_host);

    option += serialize_array_option(COAP_OPTION_URI_HOST, current_number, option, (uint8_t *) ((coap_packet_t *)packet)->uri_host, ((coap_packet_t *)packet)->uri_host_len, NULL);
    ((coap_packet_t *)packet)->option_count += 1;
    current_number = COAP_OPTION_URI_HOST;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_LOCATION_PATH)) {
    PRINTF("Location [%.*s]\n", ((coap_packet_t *)packet)->location_path_len, ((coap_packet_t *)packet)->location_path);

    uint8_t split_options = '/';

    option += serialize_array_option(COAP_OPTION_LOCATION_PATH, current_number, option, (uint8_t *) ((coap_packet_t *)packet)->location_path, ((coap_packet_t *)packet)->location_path_len, &split_options);
    ((coap_packet_t *)packet)->option_count += split_options;
    current_number = COAP_OPTION_LOCATION_PATH;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_PORT)) {
    PRINTF("Uri-Port [%u]\n", ((coap_packet_t *)packet)->uri_port);

    option += serialize_int_option(COAP_OPTION_URI_PORT, current_number, option, ((coap_packet_t *)packet)->uri_port);
    ((coap_packet_t *)packet)->option_count += 1;
    current_number = COAP_OPTION_URI_PORT;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_LOCATION_QUERY)) {
    PRINTF("Location-Query [%.*s]\n", ((coap_packet_t *)packet)->location_query_len, ((coap_packet_t *)packet)->location_query);

    uint8_t split_options = '&';

    option += serialize_array_option(COAP_OPTION_LOCATION_QUERY, current_number, option, (uint8_t *) ((coap_packet_t *)packet)->location_query, ((coap_packet_t *)packet)->location_query_len, &split_options);
    ((coap_packet_t *)packet)->option_count += split_options;
    current_number = COAP_OPTION_LOCATION_QUERY;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_PATH)) {
    PRINTF("Uri-Path [%.*s]\n", ((coap_packet_t *)packet)->uri_path_len, ((coap_packet_t *)packet)->uri_path);

    uint8_t split_options = '/';

    option += serialize_array_option(COAP_OPTION_URI_PATH, current_number, option, (uint8_t *) ((coap_packet_t *)packet)->uri_path, ((coap_packet_t *)packet)->uri_path_len, &split_options);
    ((coap_packet_t *)packet)->option_count += split_options;
    current_number = COAP_OPTION_URI_PATH;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_OBSERVE)) {
    PRINTF("Observe [%u]\n", ((coap_packet_t *)packet)->observe);

    option += serialize_int_option(COAP_OPTION_OBSERVE, current_number, option, ((coap_packet_t *)packet)->observe);
    ((coap_packet_t *)packet)->option_count += 1;
    current_number = COAP_OPTION_OBSERVE;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_TOKEN)) {
    PRINTF("Token %u [0x%02X%02X%02X%02X%02X%02X%02X%02X]\n", ((coap_packet_t *)packet)->token_len,
      ((coap_packet_t *)packet)->token[0],
      ((coap_packet_t *)packet)->token[1],
      ((coap_packet_t *)packet)->token[2],
      ((coap_packet_t *)packet)->token[3],
      ((coap_packet_t *)packet)->token[4],
      ((coap_packet_t *)packet)->token[5],
      ((coap_packet_t *)packet)->token[6],
      ((coap_packet_t *)packet)->token[7]
    ); /*FIXME always prints 8 bytes */

    option += serialize_array_option(COAP_OPTION_TOKEN, current_number, option, ((coap_packet_t *)packet)->token, ((coap_packet_t *)packet)->token_len, NULL);
    ((coap_packet_t *)packet)->option_count += 1;
    current_number = COAP_OPTION_TOKEN;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_ACCEPT)) {
    int i;
    for (i=0; i<((coap_packet_t *)packet)->accept_num; ++i)
    {
      PRINTF("Accept [%u]\n", ((coap_packet_t *)packet)->accept[i]);

      option += serialize_int_option(COAP_OPTION_ACCEPT, current_number, option, (uint32_t)((coap_packet_t *)packet)->accept[i]);
      ((coap_packet_t *)packet)->option_count += 1;
      current_number = COAP_OPTION_ACCEPT;
    }
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_IF_MATCH)) {
    PRINTF("If-Match [FIXME]\n");

    option += serialize_array_option(COAP_OPTION_IF_MATCH, current_number, option, ((coap_packet_t *)packet)->if_match, ((coap_packet_t *)packet)->if_match_len, NULL);
    ((coap_packet_t *)packet)->option_count += 1;
    current_number = COAP_OPTION_IF_MATCH;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_QUERY)) {
    PRINTF("Uri-Query [%.*s]\n", ((coap_packet_t *)packet)->uri_query_len, ((coap_packet_t *)packet)->uri_query);

    uint8_t split_options = '&';

    option += serialize_array_option(COAP_OPTION_URI_QUERY, current_number, option, (uint8_t *) ((coap_packet_t *)packet)->uri_query, ((coap_packet_t *)packet)->uri_query_len, &split_options);
    ((coap_packet_t *)packet)->option_count += split_options + (COAP_OPTION_URI_QUERY-current_number)/COAP_OPTION_FENCE_POST;
    current_number = COAP_OPTION_URI_QUERY;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_BLOCK2))
  {
    PRINTF("Block2 [%lu%s (%u B/blk)]\n", ((coap_packet_t *)packet)->block2_num, ((coap_packet_t *)packet)->block2_more ? "+" : "", ((coap_packet_t *)packet)->block2_size);

    uint32_t block = ((coap_packet_t *)packet)->block2_num << 4;
    if (((coap_packet_t *)packet)->block2_more) block |= 0x8;
    block |= 0xF & log_2(((coap_packet_t *)packet)->block2_size/16);

    PRINTF("Block2 encoded: 0x%lX\n", block);

    option += serialize_int_option(COAP_OPTION_BLOCK2, current_number, option, block);

    ((coap_packet_t *)packet)->option_count += 1 + (COAP_OPTION_BLOCK2-current_number)/COAP_OPTION_FENCE_POST;
    current_number = COAP_OPTION_BLOCK2;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_BLOCK1))
  {
    PRINTF("Block1 [%lu%s (%u B/blk)]\n", ((coap_packet_t *)packet)->block1_num, ((coap_packet_t *)packet)->block1_more ? "+" : "", ((coap_packet_t *)packet)->block1_size);

    uint32_t block = ((coap_packet_t *)packet)->block1_num << 4;
    if (((coap_packet_t *)packet)->block1_more) block |= 0x8;
    block |= 0xF & log_2(((coap_packet_t *)packet)->block1_size/16);

    PRINTF("Block1 encoded: 0x%lX\n", block);

    option += serialize_int_option(COAP_OPTION_BLOCK1, current_number, option, block);

    ((coap_packet_t *)packet)->option_count += 1 + (COAP_OPTION_BLOCK1-current_number)/COAP_OPTION_FENCE_POST;
    current_number = COAP_OPTION_BLOCK1;
  }
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_IF_NONE_MATCH)) {
    PRINTF("If-None-Match\n");

    option += serialize_int_option(COAP_OPTION_IF_NONE_MATCH, current_number, option, 0);

    ((coap_packet_t *)packet)->option_count += 1 + (COAP_OPTION_IF_NONE_MATCH-current_number)/COAP_OPTION_FENCE_POST;
    current_number = COAP_OPTION_IF_NONE_MATCH;
  }

  /* pack payload */
  if ((option - ((coap_packet_t *)packet)->buffer)<=COAP_MAX_HEADER_SIZE)
  {
    memmove(option, ((coap_packet_t *)packet)->payload, ((coap_packet_t *)packet)->payload_len);
  }
  else
  {
    /* An error occured. Caller must check for !=0. */
    ((coap_packet_t *)packet)->buffer = NULL;
    coap_error_message = "Serialized header exceeds COAP_MAX_HEADER_SIZE";
    return 0;
  }

  /* set header fields */
  ((coap_packet_t *)packet)->buffer[0]  = 0x00;
  ((coap_packet_t *)packet)->buffer[0] |= COAP_HEADER_VERSION_MASK & (((coap_packet_t *)packet)->version)<<COAP_HEADER_VERSION_POSITION;
  ((coap_packet_t *)packet)->buffer[0] |= COAP_HEADER_TYPE_MASK & (((coap_packet_t *)packet)->type)<<COAP_HEADER_TYPE_POSITION;
  ((coap_packet_t *)packet)->buffer[0] |= COAP_HEADER_OPTION_COUNT_MASK & (((coap_packet_t *)packet)->option_count)<<COAP_HEADER_OPTION_COUNT_POSITION;
  ((coap_packet_t *)packet)->buffer[1] = ((coap_packet_t *)packet)->code;
  ((coap_packet_t *)packet)->buffer[2] = 0xFF & (((coap_packet_t *)packet)->tid)>>8;
  ((coap_packet_t *)packet)->buffer[3] = 0xFF & ((coap_packet_t *)packet)->tid;

  PRINTF("-Done %u options, header len %u, payload len %u-\n", ((coap_packet_t *)packet)->option_count, option - buffer, ((coap_packet_t *)packet)->payload_len);

  return (option - buffer) + ((coap_packet_t *)packet)->payload_len; /* packet length */
}
/*-----------------------------------------------------------------------------------*/
void
coap_send_message(uip_ipaddr_t *addr, uint16_t port, uint8_t *data, uint16_t length)
{
  /*configure connection to reply to client*/
  uip_ipaddr_copy(&udp_conn->ripaddr, addr);
  udp_conn->rport = port;

  uip_udp_packet_send(udp_conn, data, length);
  PRINTF("-sent UDP datagram (%u)-\n", length);

  /* Restore server connection to allow data from any node */
  memset(&udp_conn->ripaddr, 0, sizeof(udp_conn->ripaddr));
  udp_conn->rport = 0;
}
/*-----------------------------------------------------------------------------------*/
coap_status_t
coap_parse_message(void *packet, uint8_t *data, uint16_t data_len)
{
  /* Initialize packet */
  memset(packet, 0, sizeof(coap_packet_t));

  /* pointer to packet bytes */
  ((coap_packet_t *)packet)->buffer = data;

  /* parse header fields */
  ((coap_packet_t *)packet)->version = (COAP_HEADER_VERSION_MASK & ((coap_packet_t *)packet)->buffer[0])>>COAP_HEADER_VERSION_POSITION;
  ((coap_packet_t *)packet)->type = (COAP_HEADER_TYPE_MASK & ((coap_packet_t *)packet)->buffer[0])>>COAP_HEADER_TYPE_POSITION;
  ((coap_packet_t *)packet)->option_count = (COAP_HEADER_OPTION_COUNT_MASK & ((coap_packet_t *)packet)->buffer[0])>>COAP_HEADER_OPTION_COUNT_POSITION;
  ((coap_packet_t *)packet)->code = ((coap_packet_t *)packet)->buffer[1];
  ((coap_packet_t *)packet)->tid = ((coap_packet_t *)packet)->buffer[2]<<8 | ((coap_packet_t *)packet)->buffer[3];

  if (((coap_packet_t *)packet)->version != 1)
  {
    coap_error_message = "CoAP version must be 1";
    return BAD_REQUEST_4_00;
  }

  /* parse options */
  ((coap_packet_t *)packet)->options = 0x0000;
  uint8_t *current_option = data + COAP_HEADER_LEN;

  if (((coap_packet_t *)packet)->option_count)
  {
    uint8_t option_index = 0;

    uint8_t current_number = 0;
    size_t option_len = 0;

    PRINTF("-Parsing %u options-\n", ((coap_packet_t *)packet)->option_count);
    for (option_index=0; option_index < ((coap_packet_t *)packet)->option_count; ++option_index) {

      current_number += current_option[0]>>4;

      PRINTF("OPTION %u (type %u, delta %u, len %u): ", option_index, current_number, current_option[0]>>4, (0x0F & current_option[0]) < 15 ? (0x0F & current_option[0]) : current_option[1] + 15);

      if ((0x0F & current_option[0]) < 15) {
        option_len = 0x0F & current_option[0];
        current_option += 1;
      } else {
        option_len = current_option[1] + 15;
        current_option += 2;
      }

      SET_OPTION((coap_packet_t *)packet, current_number);

      switch (current_number) {
        case COAP_OPTION_CONTENT_TYPE:
          ((coap_packet_t *)packet)->content_type = parse_int_option(current_option, option_len);
          PRINTF("Content-Type [%u]\n", ((coap_packet_t *)packet)->content_type);
          break;
        case COAP_OPTION_MAX_AGE:
          ((coap_packet_t *)packet)->max_age = parse_int_option(current_option, option_len);
          PRINTF("Max-Age [%lu]\n", ((coap_packet_t *)packet)->max_age);
          break;
        case COAP_OPTION_PROXY_URI:
          /*FIXME check for own end-point */
          ((coap_packet_t *)packet)->proxy_uri = (char *) current_option;
          ((coap_packet_t *)packet)->proxy_uri_len = option_len;
          /*TODO length > 270 not implemented (actually not required) */
          PRINTF("Proxy-Uri NOT IMPLEMENTED [%.*s]\n", ((coap_packet_t *)packet)->proxy_uri_len, ((coap_packet_t *)packet)->proxy_uri);
          coap_error_message = "This is a constrained server (Contiki)";
          return PROXYING_NOT_SUPPORTED_5_05;
          break;
        case COAP_OPTION_ETAG:
          ((coap_packet_t *)packet)->etag_len = MIN(COAP_ETAG_LEN, option_len);
          memcpy(((coap_packet_t *)packet)->etag, current_option, ((coap_packet_t *)packet)->etag_len);
          PRINTF("ETag %u [0x%02X%02X%02X%02X%02X%02X%02X%02X]\n", ((coap_packet_t *)packet)->etag_len,
            ((coap_packet_t *)packet)->etag[0],
            ((coap_packet_t *)packet)->etag[1],
            ((coap_packet_t *)packet)->etag[2],
            ((coap_packet_t *)packet)->etag[3],
            ((coap_packet_t *)packet)->etag[4],
            ((coap_packet_t *)packet)->etag[5],
            ((coap_packet_t *)packet)->etag[6],
            ((coap_packet_t *)packet)->etag[7]
          ); /*FIXME always prints 8 bytes */
          break;
        case COAP_OPTION_URI_HOST:
          ((coap_packet_t *)packet)->uri_host = (char *) current_option;
          ((coap_packet_t *)packet)->uri_host_len = option_len;
          PRINTF("Uri-Host [%.*s]\n", ((coap_packet_t *)packet)->uri_host_len, ((coap_packet_t *)packet)->uri_host);
          break;
        case COAP_OPTION_LOCATION_PATH:
          coap_merge_multi_option(&(((coap_packet_t *)packet)->location_path), &(((coap_packet_t *)packet)->location_path_len), current_option, option_len, '/');
          PRINTF("Location-Path [%.*s]\n", ((coap_packet_t *)packet)->location_path_len, ((coap_packet_t *)packet)->location_path);
          break;
        case COAP_OPTION_URI_PORT:
          ((coap_packet_t *)packet)->uri_port = parse_int_option(current_option, option_len);
          PRINTF("Uri-Port [%u]\n", ((coap_packet_t *)packet)->uri_port);
          break;
        case COAP_OPTION_LOCATION_QUERY:
          coap_merge_multi_option(&(((coap_packet_t *)packet)->location_query), &(((coap_packet_t *)packet)->location_query_len), current_option, option_len, '&');
          PRINTF("Location-Query [%.*s]\n", ((coap_packet_t *)packet)->location_query_len, ((coap_packet_t *)packet)->location_query);
          break;
        case COAP_OPTION_URI_PATH:
          coap_merge_multi_option(&(((coap_packet_t *)packet)->uri_path), &(((coap_packet_t *)packet)->uri_path_len), current_option, option_len, '/');
          PRINTF("Uri-Path [%.*s]\n", ((coap_packet_t *)packet)->uri_path_len, ((coap_packet_t *)packet)->uri_path);
          break;
        case COAP_OPTION_OBSERVE:
          ((coap_packet_t *)packet)->observe = parse_int_option(current_option, option_len);
          PRINTF("Observe [%u]\n", ((coap_packet_t *)packet)->observe);
          break;
        case COAP_OPTION_TOKEN:
          ((coap_packet_t *)packet)->token_len = MIN(COAP_TOKEN_LEN, option_len);
          memcpy(((coap_packet_t *)packet)->token, current_option, ((coap_packet_t *)packet)->token_len);
          PRINTF("Token %u [0x%02X%02X%02X%02X%02X%02X%02X%02X]\n", ((coap_packet_t *)packet)->token_len,
            ((coap_packet_t *)packet)->token[0],
            ((coap_packet_t *)packet)->token[1],
            ((coap_packet_t *)packet)->token[2],
            ((coap_packet_t *)packet)->token[3],
            ((coap_packet_t *)packet)->token[4],
            ((coap_packet_t *)packet)->token[5],
            ((coap_packet_t *)packet)->token[6],
            ((coap_packet_t *)packet)->token[7]
          ); /*FIXME always prints 8 bytes */
          break;
        case COAP_OPTION_ACCEPT:
          if (((coap_packet_t *)packet)->accept_num < COAP_MAX_ACCEPT_NUM)
          {
            ((coap_packet_t *)packet)->accept[((coap_packet_t *)packet)->accept_num] = parse_int_option(current_option, option_len);
            ((coap_packet_t *)packet)->accept_num += 1;
            PRINTF("Accept [%u]\n", ((coap_packet_t *)packet)->content_type);
          }
          break;
        case COAP_OPTION_IF_MATCH:
          /*FIXME support multiple ETags */
          ((coap_packet_t *)packet)->if_match_len = MIN(COAP_ETAG_LEN, option_len);
          memcpy(((coap_packet_t *)packet)->if_match, current_option, ((coap_packet_t *)packet)->if_match_len);
          PRINTF("If-Match %u [0x%02X%02X%02X%02X%02X%02X%02X%02X]\n", ((coap_packet_t *)packet)->if_match_len,
            ((coap_packet_t *)packet)->if_match[0],
            ((coap_packet_t *)packet)->if_match[1],
            ((coap_packet_t *)packet)->if_match[2],
            ((coap_packet_t *)packet)->if_match[3],
            ((coap_packet_t *)packet)->if_match[4],
            ((coap_packet_t *)packet)->if_match[5],
            ((coap_packet_t *)packet)->if_match[6],
            ((coap_packet_t *)packet)->if_match[7]
          ); /*FIXME always prints 8 bytes */
          break;
        case COAP_OPTION_FENCE_POST:
          PRINTF("Fence-Post\n");
          break;
        case COAP_OPTION_URI_QUERY:
          coap_merge_multi_option(&(((coap_packet_t *)packet)->uri_query), &(((coap_packet_t *)packet)->uri_query_len), current_option, option_len, '&');
          PRINTF("Uri-Query [%.*s]\n", ((coap_packet_t *)packet)->uri_query_len, ((coap_packet_t *)packet)->uri_query);
          break;
        case COAP_OPTION_BLOCK2:
          ((coap_packet_t *)packet)->block2_num = parse_int_option(current_option, option_len);
          ((coap_packet_t *)packet)->block2_more = (((coap_packet_t *)packet)->block2_num & 0x08)>>3;
          ((coap_packet_t *)packet)->block2_size = 16 << (((coap_packet_t *)packet)->block2_num & 0x07);
          ((coap_packet_t *)packet)->block2_offset = (((coap_packet_t *)packet)->block2_num & ~0x0000000F)<<(((coap_packet_t *)packet)->block2_num & 0x07);
          ((coap_packet_t *)packet)->block2_num >>= 4;
          PRINTF("Block2 [%lu%s (%u B/blk)]\n", ((coap_packet_t *)packet)->block2_num, ((coap_packet_t *)packet)->block2_more ? "+" : "", ((coap_packet_t *)packet)->block2_size);
          break;
        case COAP_OPTION_BLOCK1:
          PRINTF("Block1 NOT IMPLEMENTED\n");
          /*TODO implement */
          coap_error_message = "Blockwise POST/PUT not supported";
          return NOT_IMPLEMENTED_5_01;
          break;
        case COAP_OPTION_IF_NONE_MATCH:
          ((coap_packet_t *)packet)->if_none_match = 1;
          PRINTF("If-None-Match\n");
          break;
        default:
          PRINTF("unknown (%u)\n", current_number);
          /* Check if critical (odd) */
          if (current_number & 1)
          {
            coap_error_message = "Unsupported critical option";
            return BAD_OPTION_4_02;
          }
      }

      current_option += option_len;
    } /* for */
    PRINTF("-Done parsing-------\n");
  } /* if (oc) */

  ((coap_packet_t *)packet)->payload = current_option;
  ((coap_packet_t *)packet)->payload_len = data_len - (((coap_packet_t *)packet)->payload - data);

  return NO_ERROR;
}
/*-----------------------------------------------------------------------------------*/
/*- REST FRAMEWORK FUNCTIONS --------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
int
coap_get_query_variable(void *packet, const char *name, const char **output)
{
  if (IS_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_QUERY)) {
    return coap_get_variable(((coap_packet_t *)packet)->uri_query, ((coap_packet_t *)packet)->uri_query_len, name, output);
  }
  return 0;
}

int
coap_get_post_variable(void *packet, const char *name, const char **output)
{
  if (((coap_packet_t *)packet)->payload_len) {
    return coap_get_variable((const char *)((coap_packet_t *)packet)->payload, ((coap_packet_t *)packet)->payload_len, name, output);
  }
  return 0;
}
/*-----------------------------------------------------------------------------------*/
/*- HEADER OPTION GETTERS AND SETTERS -----------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
unsigned int
coap_get_header_content_type(void *packet)
{
  return ((coap_packet_t *)packet)->content_type;
}

int
coap_set_header_content_type(void *packet, unsigned int content_type)
{
  ((coap_packet_t *)packet)->content_type = (coap_content_type_t) content_type;
  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_CONTENT_TYPE);
  return 1;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_accept(void *packet, uint16_t **accept)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_ACCEPT)) return 0;

  *accept = ((coap_packet_t *)packet)->accept;
  return ((coap_packet_t *)packet)->accept_num;
}

int
coap_set_header_accept(void *packet, uint16_t accept)
{
  if (((coap_packet_t *)packet)->accept_num < COAP_MAX_ACCEPT_NUM)
  {
    ((coap_packet_t *)packet)->accept[((coap_packet_t *)packet)->accept_num] = accept;
    ((coap_packet_t *)packet)->accept_num += 1;

    SET_OPTION((coap_packet_t *)packet, COAP_OPTION_ACCEPT);
  }
  return ((coap_packet_t *)packet)->accept_num;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_max_age(void *packet, uint32_t *age)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_MAX_AGE)) {
    *age = COAP_DEFAULT_MAX_AGE;
  } else {
    *age = ((coap_packet_t *)packet)->max_age;
  }
  return 1;
}

int
coap_set_header_max_age(void *packet, uint32_t age)
{
  ((coap_packet_t *)packet)->max_age = age;
  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_MAX_AGE);
  return 1;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_etag(void *packet, const uint8_t **etag)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_ETAG)) return 0;

  *etag = ((coap_packet_t *)packet)->etag;
  return ((coap_packet_t *)packet)->etag_len;
}

int
coap_set_header_etag(void *packet, uint8_t *etag, size_t etag_len)
{
  ((coap_packet_t *)packet)->etag_len = MIN(COAP_ETAG_LEN, etag_len);
  memcpy(((coap_packet_t *)packet)->etag, etag, ((coap_packet_t *)packet)->etag_len);

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_ETAG);
  return ((coap_packet_t *)packet)->etag_len;
}
/*-----------------------------------------------------------------------------------*/
/*FIXME support multiple ETags */
int
coap_get_header_if_match(void *packet, const uint8_t **etag)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_IF_MATCH)) return 0;

  *etag = ((coap_packet_t *)packet)->if_match;
  return ((coap_packet_t *)packet)->if_match_len;
}

int
coap_set_header_if_match(void *packet, uint8_t *etag, size_t etag_len)
{
  ((coap_packet_t *)packet)->if_match_len = MIN(COAP_ETAG_LEN, etag_len);
  memcpy(((coap_packet_t *)packet)->if_match, etag, ((coap_packet_t *)packet)->if_match_len);

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_IF_MATCH);
  return ((coap_packet_t *)packet)->if_match_len;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_if_none_match(void *packet)
{
  return IS_OPTION((coap_packet_t *)packet, COAP_OPTION_IF_NONE_MATCH) ? 1 : 0;
}

int
coap_set_header_if_none_match(void *packet)
{
  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_IF_NONE_MATCH);
  return 1;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_token(void *packet, const uint8_t **token)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_TOKEN)) return 0;

  *token = ((coap_packet_t *)packet)->token;
  return ((coap_packet_t *)packet)->token_len;
}

int
coap_set_header_token(void *packet, uint8_t *token, size_t token_len)
{
  ((coap_packet_t *)packet)->token_len = MIN(COAP_TOKEN_LEN, token_len);
  memcpy(((coap_packet_t *)packet)->token, token, ((coap_packet_t *)packet)->token_len);

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_TOKEN);
  return ((coap_packet_t *)packet)->token_len;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_proxy_uri(void *packet, const char **uri)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_PROXY_URI)) return 0;

  *uri = ((coap_packet_t *)packet)->proxy_uri;
  return ((coap_packet_t *)packet)->proxy_uri_len;
}

int
coap_set_header_proxy_uri(void *packet, char *uri)
{
  ((coap_packet_t *)packet)->proxy_uri = uri;
  ((coap_packet_t *)packet)->proxy_uri_len = strlen(uri);

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_PROXY_URI);
  return ((coap_packet_t *)packet)->proxy_uri_len;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_uri_host(void *packet, const char **host)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_HOST)) return 0;

  *host = ((coap_packet_t *)packet)->uri_host;
  return ((coap_packet_t *)packet)->uri_host_len;
}

int
coap_set_header_uri_host(void *packet, char *host)
{
  ((coap_packet_t *)packet)->uri_host = host;
  ((coap_packet_t *)packet)->uri_host_len = strlen(host);

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_HOST);
  return ((coap_packet_t *)packet)->uri_host_len;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_uri_path(void *packet, const char **path)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_PATH)) return 0;

  *path = ((coap_packet_t *)packet)->uri_path;
  return ((coap_packet_t *)packet)->uri_path_len;
}

int
coap_set_header_uri_path(void *packet, char *path)
{
  while (path[0]=='/') ++path;

  ((coap_packet_t *)packet)->uri_path = path;
  ((coap_packet_t *)packet)->uri_path_len = strlen(path);

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_PATH);
  return ((coap_packet_t *)packet)->uri_path_len;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_uri_query(void *packet, const char **query)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_QUERY)) return 0;

  *query = ((coap_packet_t *)packet)->uri_query;
  return ((coap_packet_t *)packet)->uri_query_len;
}

int
coap_set_header_uri_query(void *packet, char *query)
{
  while (query[0]=='?') ++query;

  ((coap_packet_t *)packet)->uri_query = query;
  ((coap_packet_t *)packet)->uri_query_len = strlen(query);

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_URI_QUERY);
  return ((coap_packet_t *)packet)->uri_query_len;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_location_path(void *packet, const char **path)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_LOCATION_PATH)) return 0;

  *path = ((coap_packet_t *)packet)->location_path;
  return ((coap_packet_t *)packet)->location_path_len;
}

int
coap_set_header_location_path(void *packet, char *path)
{
  char *query;

  while (path[0]=='/') ++path;

  if ((query = strchr(path, '?')))
  {
    coap_set_header_location_query(packet, query+1);
    ((coap_packet_t *)packet)->location_path_len = query - path;
  }
  else
  {
    ((coap_packet_t *)packet)->location_path_len = strlen(path);
  }

  ((coap_packet_t *)packet)->location_path = path;

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_LOCATION_PATH);
  return ((coap_packet_t *)packet)->location_path_len;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_location_query(void *packet, const char **query)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_LOCATION_QUERY)) return 0;

  *query = ((coap_packet_t *)packet)->location_query;
  return ((coap_packet_t *)packet)->location_query_len;
}

int
coap_set_header_location_query(void *packet, char *query)
{
  while (query[0]=='?') ++query;

  ((coap_packet_t *)packet)->location_query = query;
  ((coap_packet_t *)packet)->location_query_len = strlen(query);

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_LOCATION_QUERY);
  return ((coap_packet_t *)packet)->location_query_len;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_observe(void *packet, uint32_t *observe)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_OBSERVE)) return 0;

  *observe = ((coap_packet_t *)packet)->observe;
  return 1;
}

int
coap_set_header_observe(void *packet, uint32_t observe)
{
  ((coap_packet_t *)packet)->observe = observe;
  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_OBSERVE);
  return 1;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_block2(void *packet, uint32_t *num, uint8_t *more, uint16_t *size, uint32_t *offset)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_BLOCK2)) return 0;

  /* pointers may be NULL to get only specific block parameters */
  if (num!=NULL) *num = ((coap_packet_t *)packet)->block2_num;
  if (more!=NULL) *more = ((coap_packet_t *)packet)->block2_more;
  if (size!=NULL) *size = ((coap_packet_t *)packet)->block2_size;
  if (offset!=NULL) *offset = ((coap_packet_t *)packet)->block2_offset;

  return 1;
}

int
coap_set_header_block2(void *packet, uint32_t num, uint8_t more, uint16_t size)
{
  if (size<16) return 0;
  if (size>2048) return 0;
  if (num>0x0FFFFF) return 0;

  ((coap_packet_t *)packet)->block2_num = num;
  ((coap_packet_t *)packet)->block2_more = more ? 1 : 0;
  ((coap_packet_t *)packet)->block2_size = size;

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_BLOCK2);
  return 1;
}
/*-----------------------------------------------------------------------------------*/
int
coap_get_header_block1(void *packet, uint32_t *num, uint8_t *more, uint16_t *size, uint32_t *offset)
{
  if (!IS_OPTION((coap_packet_t *)packet, COAP_OPTION_BLOCK1)) return 0;

  /* pointers may be NULL to get only specific block parameters */
  if (num!=NULL) *num = ((coap_packet_t *)packet)->block1_num;
  if (more!=NULL) *more = ((coap_packet_t *)packet)->block1_more;
  if (size!=NULL) *size = ((coap_packet_t *)packet)->block1_size;
  if (offset!=NULL) *offset = ((coap_packet_t *)packet)->block1_offset;

  return 1;
}

int
coap_set_header_block1(void *packet, uint32_t num, uint8_t more, uint16_t size)
{
  if (size<16) return 0;
  if (size>2048) return 0;
  if (num>0x0FFFFF) return 0;

  ((coap_packet_t *)packet)->block1_num = num;
  ((coap_packet_t *)packet)->block1_more = more;
  ((coap_packet_t *)packet)->block1_size = size;

  SET_OPTION((coap_packet_t *)packet, COAP_OPTION_BLOCK1);
  return 1;
}
/*-----------------------------------------------------------------------------------*/
/*- PAYLOAD -------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
int
coap_get_payload(void *packet, const uint8_t **payload)
{
  if (((coap_packet_t *)packet)->payload) {
    *payload = ((coap_packet_t *)packet)->payload;
    return ((coap_packet_t *)packet)->payload_len;
  } else {
    *payload = NULL;
    return 0;
  }
}

int
coap_set_payload(void *packet, uint8_t *payload, size_t length)
{
  PRINTF("setting payload (%u/%u)\n", length, REST_MAX_CHUNK_SIZE);

  ((coap_packet_t *)packet)->payload = payload;
  ((coap_packet_t *)packet)->payload_len = MIN(REST_MAX_CHUNK_SIZE, length);

  return ((coap_packet_t *)packet)->payload_len;
}
/*-----------------------------------------------------------------------------------*/

// {# er-coap-transactions.c #}
#include "contiki.h"
#include "contiki-net.h"

// {#
//#include "er-coap-07-transactions.h"
//#include "er-coap-07-observing.h"
#include "coap.h"
// #}

#if 0 // {# moved to app
/*
 * Modulo mask (+1 and +0.5 for rounding) for a random number to get the tick number for the random
 * retransmission time between COAP_RESPONSE_TIMEOUT and COAP_RESPONSE_TIMEOUT*COAP_RESPONSE_RANDOM_FACTOR.
 */
#define COAP_RESPONSE_TIMEOUT_TICKS         (CLOCK_SECOND * COAP_RESPONSE_TIMEOUT)
#define COAP_RESPONSE_TIMEOUT_BACKOFF_MASK  ((CLOCK_SECOND * COAP_RESPONSE_TIMEOUT * (COAP_RESPONSE_RANDOM_FACTOR - 1)) + 1.5)
#endif // #}

#if 0 // {# already defined
#define DEBUG 0
#if DEBUG
#include <stdio.h>
#define PRINTF(...) printf(__VA_ARGS__)
#define PRINT6ADDR(addr) PRINTF("[%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x]", ((u8_t *)addr)[0], ((u8_t *)addr)[1], ((u8_t *)addr)[2], ((u8_t *)addr)[3], ((u8_t *)addr)[4], ((u8_t *)addr)[5], ((u8_t *)addr)[6], ((u8_t *)addr)[7], ((u8_t *)addr)[8], ((u8_t *)addr)[9], ((u8_t *)addr)[10], ((u8_t *)addr)[11], ((u8_t *)addr)[12], ((u8_t *)addr)[13], ((u8_t *)addr)[14], ((u8_t *)addr)[15])
#define PRINTLLADDR(lladdr) PRINTF("[%02x:%02x:%02x:%02x:%02x:%02x]",(lladdr)->addr[0], (lladdr)->addr[1], (lladdr)->addr[2], (lladdr)->addr[3],(lladdr)->addr[4], (lladdr)->addr[5])
#else
#define PRINTF(...)
#define PRINT6ADDR(addr)
#define PRINTLLADDR(addr)
#endif
#endif // #}

// {# need to share with app but LIST macro creates static identifiers
//LIST(transactions_list);
//MEMB(transactions_memb, coap_transaction_t, COAP_MAX_OPEN_TRANSACTIONS);
char CC_CONCAT(transactions_memb,_memb_count)[COAP_MAX_OPEN_TRANSACTIONS];
coap_transaction_t CC_CONCAT(transactions_memb,_memb_mem)[COAP_MAX_OPEN_TRANSACTIONS];
struct memb transactions_memb = {sizeof(coap_transaction_t), COAP_MAX_OPEN_TRANSACTIONS,
                                  CC_CONCAT(transactions_memb,_memb_count),
                                  (void *)CC_CONCAT(transactions_memb,_memb_mem)};

void *LIST_CONCAT(transactions_list,_list) = NULL;
list_t transactions_list = (list_t)&LIST_CONCAT(transactions_list,_list);
// #}


#if 0 // {# moved to app
static struct process *transaction_handler_process = NULL;

void
coap_register_as_transaction_handler()
{
  transaction_handler_process = PROCESS_CURRENT();
}
#endif // #}

coap_transaction_t *
coap_new_transaction(uint16_t tid, uip_ipaddr_t *addr, uint16_t port)
{
  coap_transaction_t *t = memb_alloc(&transactions_memb);

  if (t)
  {
    t->tid = tid;
    t->retrans_counter = 0;

    /* save client address */
    uip_ipaddr_copy(&t->addr, addr);
    t->port = port;
  }

  return t;
}

#if 0 // {# moved to app
void
coap_send_transaction(coap_transaction_t *t)
{
  PRINTF("Sending transaction %u\n", t->tid);

  coap_send_message(&t->addr, t->port, t->packet, t->packet_len);

  if (COAP_TYPE_CON==((COAP_HEADER_TYPE_MASK & t->packet[0])>>COAP_HEADER_TYPE_POSITION))
  {
    if (t->retrans_counter<COAP_MAX_RETRANSMIT)
    {
      PRINTF("Keeping transaction %u\n", t->tid);

      if (t->retrans_counter==0)
      {
        t->retrans_timer.timer.interval = COAP_RESPONSE_TIMEOUT_TICKS + (random_rand() % (clock_time_t) COAP_RESPONSE_TIMEOUT_BACKOFF_MASK);
        PRINTF("Initial interval %f\n", (float)t->retrans_timer.timer.interval/CLOCK_SECOND);
      }
      else
      {
        t->retrans_timer.timer.interval <<= 1; /* double */
        PRINTF("Doubled (%u) interval %f\n", t->retrans_counter, (float)t->retrans_timer.timer.interval/CLOCK_SECOND);
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
      PRINTF("Timeout\n");
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
#endif // #}

#if 0 // {# moved to app
void
coap_clear_transaction(coap_transaction_t *t)
{
  if (t)
  {
    PRINTF("Freeing transaction %u: %p\n", t->tid, t);

    etimer_stop(&t->retrans_timer);
    list_remove(transactions_list, t);
    memb_free(&transactions_memb, t);
  }
}
#endif // #}

coap_transaction_t *
coap_get_transaction_by_tid(uint16_t tid)
{
  coap_transaction_t *t = NULL;

  for (t = (coap_transaction_t*)list_head(transactions_list); t; t = t->next)
  {
    if (t->tid==tid)
    {
      PRINTF("Found transaction for TID %u: %p\n", t->tid, t);
      return t;
    }
  }
  return NULL;
}

#if 0 // {# moved to app
void
coap_check_transactions()
{
  coap_transaction_t *t = NULL;

  for (t = (coap_transaction_t*)list_head(transactions_list); t; t = t->next)
  {
    if (etimer_expired(&t->retrans_timer))
    {
      ++(t->retrans_counter);
      PRINTF("Retransmitting %u (%u)\n", t->tid, t->retrans_counter);
      coap_send_transaction(t);
    }
  }
}
#endif // #}

// {# er-coap-engine.c #}
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "contiki.h"
#include "contiki-net.h"

// {#
// #include "er-coap-07-engine.h"
#include "coap.h"

#if 0 // {# already defined
#define DEBUG 0 
#if DEBUG
#define PRINTF(...) printf(__VA_ARGS__)
#define PRINT6ADDR(addr) PRINTF("[%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x]", ((u8_t *)addr)[0], ((u8_t *)addr)[1], ((u8_t *)addr)[2], ((u8_t *)addr)[3], ((u8_t *)addr)[4], ((u8_t *)addr)[5], ((u8_t *)addr)[6], ((u8_t *)addr)[7], ((u8_t *)addr)[8], ((u8_t *)addr)[9], ((u8_t *)addr)[10], ((u8_t *)addr)[11], ((u8_t *)addr)[12], ((u8_t *)addr)[13], ((u8_t *)addr)[14], ((u8_t *)addr)[15])
#define PRINTLLADDR(lladdr) PRINTF(" %02x:%02x:%02x:%02x:%02x:%02x ",(lladdr)->addr[0], (lladdr)->addr[1], (lladdr)->addr[2], (lladdr)->addr[3],(lladdr)->addr[4], (lladdr)->addr[5])
#define PRINTBITS(buf,len) { \
      int i,j=0; \
      for (i=0; i<len; ++i) { \
        for (j=7; j>=0; --j) { \
          PRINTF("%c", (((char *)buf)[i] & 1<<j) ? '1' : '0'); \
        } \
        PRINTF(" "); \
      } \
    }
#else
#define PRINTF(...)
#define PRINT6ADDR(addr)
#define PRINTLLADDR(addr)
#define PRINTBITS(buf,len)
#endif
#endif // #}{#
#if DEBUG
#define PRINTBITS(buf,len) { \
      int i,j=0; \
      for (i=0; i<len; ++i) { \
        for (j=7; j>=0; --j) { \
          PRINTF("%c", (((char *)buf)[i] & 1<<j) ? '1' : '0'); \
        } \
        PRINTF(" "); \
      } \
    }
#else
#define PRINTBITS(buf,len)
#endif
// #}

// PROCESS(coap_receiver, "CoAP Receiver"); {# moved to app #}

/*-----------------------------------------------------------------------------------*/
/*- Variables -----------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
static service_callback_t service_cbk = NULL;
/*-----------------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
// static {# needed from app #}
int
handle_incoming_data(void)
{
  coap_error_code = NO_ERROR;

  PRINTF("handle_incoming_data(): received uip_datalen=%u \n",(uint16_t)uip_datalen());

  /* Static declaration reduces stack peaks and program code size. */
  static coap_packet_t message[1]; /* This way the packet can be treated as pointer as usual. */
  static coap_packet_t response[1];
  static coap_transaction_t *transaction = NULL;

  if (uip_newdata()) {

    PRINTF("receiving UDP datagram from: ");
    PRINT6ADDR(&UIP_IP_BUF->srcipaddr);
    PRINTF(":%u\n  Length: %u\n  Data: ", uip_ntohs(UIP_UDP_BUF->srcport), uip_datalen() );
    PRINTBITS(uip_appdata, uip_datalen());
    PRINTF("\n");

    coap_error_code = coap_parse_message(message, uip_appdata, uip_datalen());

    if (coap_error_code==NO_ERROR)
    {

      /*TODO duplicates suppression, if required */

      PRINTF("  Parsed: v %u, t %u, oc %u, c %u, tid %u\n", message->version, message->type, message->option_count, message->code, message->tid);
      PRINTF("  URL: %.*s\n", message->uri_path_len, message->uri_path);
      PRINTF("  Payload: %.*s\n", message->payload_len, message->payload);

      /* Handle requests. */
      if (message->code >= COAP_GET && message->code <= COAP_DELETE)
      {
        /* Use transaction buffer for response to confirmable request. */
        if ( (transaction = coap_new_transaction(message->tid, &UIP_IP_BUF->srcipaddr, UIP_UDP_BUF->srcport)) )
        {
          static uint32_t block_num = 0;
          static uint16_t block_size = REST_MAX_CHUNK_SIZE;
          static uint32_t block_offset = 0;
          static int32_t new_offset = 0;

          /* prepare response */
          if (message->type==COAP_TYPE_CON)
          {
            /* Reliable CON requests are answered with an ACK. */
            coap_init_message(response, COAP_TYPE_ACK, CONTENT_2_05, message->tid);
          }
          else
          {
            /* Unreliable NON requests are answered with a NON as well. */
            coap_init_message(response, COAP_TYPE_NON, CONTENT_2_05, coap_get_tid());
          }

          /* resource handlers must take care of different handling (e.g., TOKEN_OPTION_REQUIRED_240) */
          if (IS_OPTION(message, COAP_OPTION_TOKEN))
          {
              coap_set_header_token(response, message->token, message->token_len);
              SET_OPTION(response, COAP_OPTION_TOKEN);
          }

          /* get offset for blockwise transfers */
          if (coap_get_header_block2(message, &block_num, NULL, &block_size, &block_offset))
          {
              PRINTF("Blockwise: block request %lu (%u/%u) @ %lu bytes\n", block_num, block_size, REST_MAX_CHUNK_SIZE, block_offset);
              block_size = MIN(block_size, REST_MAX_CHUNK_SIZE);
              new_offset = block_offset;
          }
          else
          {
            block_size = REST_MAX_CHUNK_SIZE;
            new_offset = 0;
          }

          /* Invoke resource handler. */
          if (service_cbk)
          {
            /* Call REST framework and check if found and allowed. */
            if (service_cbk(message, response, transaction->packet+COAP_MAX_HEADER_SIZE, block_size, &new_offset))
            {
              /* Apply blockwise transfers. */
              if ( IS_OPTION(message, COAP_OPTION_BLOCK2) )
              {
                /* unchanged new_offset indicates that resource is unaware of blockwise transfer */
                if (new_offset==block_offset)
                {
                  PRINTF("Blockwise: unaware resource with payload length %u/%u\n", response->payload_len, block_size);
                  if (block_offset >= response->payload_len)
                  {
                    PRINTF("handle_incoming_data(): block_offset >= response->payload_len\n");

                    response->code = BAD_OPTION_4_02;
                    coap_set_payload(response, (uint8_t*)"BlockOutOfScope", 15);
                  }
                  else
                  {
                    coap_set_header_block2(response, block_num, response->payload_len - block_offset > block_size, block_size);
                    coap_set_payload(response, response->payload+block_offset, MIN(response->payload_len - block_offset, block_size));
                  } /* if (valid offset) */
                }
                else
                {
                  /* resource provides chunk-wise data */
                  PRINTF("Blockwise: blockwise resource, new offset %ld\n", new_offset);
                  coap_set_header_block2(response, block_num, new_offset!=-1 || response->payload_len > block_size, block_size);
                  if (response->payload_len > block_size) coap_set_payload(response, response->payload, block_size);
                } /* if (resource aware of blockwise) */
              }
              else if (new_offset!=0)
              {
                PRINTF("Blockwise: no block option for blockwise resource, using block size %u\n", REST_MAX_CHUNK_SIZE);

                coap_set_header_block2(response, 0, new_offset!=-1, REST_MAX_CHUNK_SIZE);
                coap_set_payload(response, response->payload, MIN(response->payload_len, REST_MAX_CHUNK_SIZE));
              } /* if (blockwise request) */
            }
          }
          else
          {
            coap_error_code = INTERNAL_SERVER_ERROR_5_00;
            coap_error_message = "Service callback undefined";
          } /* if (service callback) */

          /* serialize Response. */
          if ((transaction->packet_len = coap_serialize_message(response, transaction->packet))==0)
          {
            coap_error_code = PACKET_SERIALIZATION_ERROR;
          }

        } else {
            coap_error_code = MEMORY_ALLOC_ERR;
            coap_error_message = "Transaction buffer allocation failed";
        } /* if (transaction buffer) */
      }
      else
      {
        /* Responses */

        if (message->type==COAP_TYPE_ACK)
        {
          PRINTF("Received ACK\n");
        }
        else if (message->type==COAP_TYPE_RST)
        {
          PRINTF("Received RST\n");
          /* Cancel possible subscriptions. */
          if (IS_OPTION(message, COAP_OPTION_TOKEN))
          {
            PRINTF("  Token 0x%02X%02X\n", message->token[0], message->token[1]);
            coap_remove_observer_by_token(&UIP_IP_BUF->srcipaddr, UIP_UDP_BUF->srcport, message->token, message->token_len);
          }
        }

        if ( (transaction = coap_get_transaction_by_tid(message->tid)) )
        {
          /* Free transaction memory before callback, as it may create a new transaction. */
          restful_response_handler callback = transaction->callback;
          void *callback_data = transaction->callback_data;
          coap_clear_transaction(transaction);

          /* Check if someone registered for the response */
          if (callback) {
            callback(callback_data, message);
          }
        } /* if (ACKed transaction) */
        transaction = NULL;
      }
    } /* if (parsed correctly) */

    if (coap_error_code==NO_ERROR) {
      if (transaction) coap_send_transaction(transaction);
    }
    else
    {
      PRINTF("ERROR %u: %s\n", coap_error_code, coap_error_message);
      coap_clear_transaction(transaction);

      /* Set to sendable error code. */
      if (coap_error_code >= 192)
      {
        coap_error_code = INTERNAL_SERVER_ERROR_5_00;
      }
      /* Reuse input buffer for error message. */
      coap_init_message(message, COAP_TYPE_ACK, coap_error_code, message->tid);
      coap_set_payload(message, (uint8_t *) coap_error_message, strlen(coap_error_message));
      coap_send_message(&UIP_IP_BUF->srcipaddr, UIP_UDP_BUF->srcport, uip_appdata, coap_serialize_message(message, uip_appdata));
    }
  } /* if (new data) */

  return coap_error_code;
}
/*-----------------------------------------------------------------------------------*/
#if 0 // {# moved to app
void
coap_receiver_init()
{
  process_start(&coap_receiver, NULL);
}
#endif // #}
/*-----------------------------------------------------------------------------------*/
void
coap_set_service_callback(service_callback_t callback)
{
  service_cbk = callback;
}
/*-----------------------------------------------------------------------------------*/
#if 0 // {# we don't use erbium
rest_resource_flags_t
coap_get_rest_method(void *packet)
{
  return (rest_resource_flags_t)(1 << (((coap_packet_t *)packet)->code - 1));
}
#endif // #}
/*-----------------------------------------------------------------------------------*/
int
coap_set_rest_status(void *packet, unsigned int code)
{
  if (code <= 0xFF)
  {
    ((coap_packet_t *)packet)->code = (uint8_t) code;
    return 1;
  }
  else
  {
    return 0;
  }
}
#if 0 // {# clients don't have resources
/*-----------------------------------------------------------------------------------*/
/*- Server part ---------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
/* The discover resource is automatically included for CoAP. */
RESOURCE(well_known_core, METHOD_GET, ".well-known/core", "ct=40");
void
well_known_core_handler(void* request, void* response, uint8_t *buffer, uint16_t preferred_size, int32_t *offset)
{
    size_t strpos = 0;
    size_t bufpos = 0;
    size_t tmplen = 0;
    resource_t* resource = NULL;

    for (resource = (resource_t*)list_head(rest_get_resources()); resource; resource = resource->next)
    {
      PRINTF("res: /%s (%p)\npos: s%d, o%d, b%d\n", resource->url, resource, strpos, *offset, bufpos);

      if (strpos >= *offset && bufpos < preferred_size)
      {
        buffer[bufpos++] = '<';
      }
      ++strpos;

      if (strpos >= *offset && bufpos < preferred_size)
      {
        buffer[bufpos++] = '/';
      }
      ++strpos;

      tmplen = strlen(resource->url);
      if (strpos+tmplen > *offset)
      {
        bufpos += snprintf((char *) buffer + bufpos, preferred_size - bufpos + 1,
                         "%s", resource->url + ((*offset-(int32_t)strpos > 0) ? (*offset-(int32_t)strpos) : 0));
                                                          /* minimal-net requires these casts */
      }
      strpos += tmplen;

      if (strpos >= *offset && bufpos < preferred_size)
      {
        buffer[bufpos++] = '>';
      }
      ++strpos;

      if (resource->attributes[0])
      {
        if (strpos >= *offset && bufpos < preferred_size)
        {
          buffer[bufpos++] = ';';
        }
        ++strpos;

        tmplen = strlen(resource->attributes);
        if (strpos+tmplen > *offset)
        {
          bufpos += snprintf((char *) buffer + bufpos, preferred_size - bufpos + 1,
                         "%s", resource->attributes + (*offset-(int32_t)strpos > 0 ? *offset-(int32_t)strpos : 0));
        }
        strpos += tmplen;
      }

      if (resource->next)
      {
        if (strpos >= *offset && bufpos < preferred_size)
        {
          buffer[bufpos++] = ',';
        }
        ++strpos;
      }

      /* buffer full, but resource not completed yet; or: do not break if resource exactly fills buffer. */
      if (bufpos >= preferred_size && strpos-bufpos > *offset)
      {
        PRINTF("res: BREAK at %s (%p)\n", resource->url, resource);
        break;
      }
    }

    if (bufpos>0) {
      PRINTF("BUF %d: %.*s\n", bufpos, bufpos, (char *) buffer);

      coap_set_payload(response, buffer, bufpos );
      coap_set_header_content_type(response, APPLICATION_LINK_FORMAT);
    }
    else
    {
      PRINTF("well_known_core_handler(): bufpos<=0\n");

      coap_set_rest_status(response, BAD_OPTION_4_02);
      coap_set_payload(response, (uint8_t*)"BlockOutOfScope", 15);
    }

    if (resource==NULL) {
      PRINTF("res: DONE\n");
      *offset = -1;
    }
    else
    {
      PRINTF("res: MORE at %s (%p)\n", resource->url, resource);
      *offset += bufpos;
    }
}
#endif // #}
/*-----------------------------------------------------------------------------------*/
#if 0 // {# moved to app
PROCESS_THREAD(coap_receiver, ev, data)
{
  PROCESS_BEGIN();
  PRINTF("Starting CoAP-07 receiver...\n");

  // rest_activate_resource(&resource_well_known_core); {# clients don't have resources #}

  coap_register_as_transaction_handler();
  coap_init_connection(SERVER_LISTEN_PORT);
  PRINTF("Listening on port %u\n", UIP_HTONS(SERVER_LISTEN_PORT));

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
#endif // #}
#if 0 // {# moved to app
/*-----------------------------------------------------------------------------------*/
/*- Client part ---------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
void blocking_request_callback(void *callback_data, void *response) {
  struct request_state_t *state = (struct request_state_t *) callback_data;
  state->response = (coap_packet_t*) response;
  process_poll(state->process);
}
/*-----------------------------------------------------------------------------------*/
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
      PRINTF("Requested #%lu (TID %u)\n", state->block_num, request->tid);

      PT_YIELD_UNTIL(&state->pt, ev == PROCESS_EVENT_POLL);

      if (!state->response)
      {
        PRINTF("Server not responding\n");
        PT_EXIT(&state->pt);
      }

      coap_get_header_block2(state->response, &res_block, &more, NULL, NULL);

      PRINTF("Received #%lu%s (%u bytes)\n", res_block, more ? "+" : "", state->response->payload_len);

      if (res_block==state->block_num)
      {
        request_callback(state->response);
        ++(state->block_num);
      }
      else
      {
        PRINTF("WRONG BLOCK %lu/%lu\n", res_block, state->block_num);
        ++block_error;
      }
    }
    else
    {
      PRINTF("Could not allocate transaction buffer");
      PT_EXIT(&state->pt);
    }
  } while (more && block_error<COAP_MAX_ATTEMPTS);

  PT_END(&state->pt);
}
#endif // #}
#if 0 // {# we don't use erbium
/*-----------------------------------------------------------------------------------*/
/*- Engine Interface ----------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
const struct rest_implementation coap_rest_implementation = {
  "CoAP-07",

  coap_receiver_init,
  coap_set_service_callback,

  coap_get_header_uri_path,
  coap_set_header_uri_path,
  coap_get_rest_method,
  coap_set_rest_status,

  coap_get_header_content_type,
  coap_set_header_content_type,
  coap_get_header_accept,
  coap_get_header_max_age,
  coap_set_header_max_age,
  coap_set_header_etag,
  coap_get_header_if_match,
  coap_get_header_if_none_match,
  coap_get_header_uri_host,
  coap_set_header_location_path,

  coap_get_payload,
  coap_set_payload,

  coap_get_header_uri_query,
  coap_get_query_variable,
  coap_get_post_variable,

  coap_notify_observers,
  (restful_post_handler) coap_observe_handler,

  NULL, /* default pre-handler (set separate handler after activation if needed) */
  NULL, /* default post-handler for non-observable resources */

  {
    CONTENT_2_05,
    CREATED_2_01,
    CHANGED_2_04,
    DELETED_2_02,
    VALID_2_03,
    BAD_REQUEST_4_00,
    UNAUTHORIZED_4_01,
    BAD_OPTION_4_02,
    FORBIDDEN_4_03,
    NOT_FOUND_4_04,
    METHOD_NOT_ALLOWED_4_05,
    REQUEST_ENTITY_TOO_LARGE_4_13,
    UNSUPPORTED_MADIA_TYPE_4_15,
    INTERNAL_SERVER_ERROR_5_00,
    NOT_IMPLEMENTED_5_01,
    BAD_GATEWAY_5_02,
    SERVICE_UNAVAILABLE_5_03,
    GATEWAY_TIMEOUT_5_04,
    PROXYING_NOT_SUPPORTED_5_05
  },

  {
    TEXT_PLAIN,
    TEXT_XML,
    TEXT_CSV,
    TEXT_HTML,
    IMAGE_GIF,
    IMAGE_JPEG,
    IMAGE_PNG,
    IMAGE_TIFF,
    AUDIO_RAW,
    VIDEO_RAW,
    APPLICATION_LINK_FORMAT,
    APPLICATION_XML,
    APPLICATION_OCTET_STREAM,
    APPLICATION_RDF_XML,
    APPLICATION_SOAP_XML,
    APPLICATION_ATOM_XML,
    APPLICATION_XMPP_XML,
    APPLICATION_EXI,
    APPLICATION_FASTINFOSET,
    APPLICATION_SOAP_FASTINFOSET,
    APPLICATION_JSON,
    APPLICATION_X_OBIX_BINARY
  }
};
#endif // #}

// {# er-coap-07-observing.c #}
#include <stdio.h>
#include <string.h>

// #include "er-coap-07-observing.h" {##}

#if 0 // {# already defined
#define DEBUG 0
#if DEBUG
#define PRINTF(...) printf(__VA_ARGS__)
#define PRINT6ADDR(addr) PRINTF("[%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x]", ((u8_t *)addr)[0], ((u8_t *)addr)[1], ((u8_t *)addr)[2], ((u8_t *)addr)[3], ((u8_t *)addr)[4], ((u8_t *)addr)[5], ((u8_t *)addr)[6], ((u8_t *)addr)[7], ((u8_t *)addr)[8], ((u8_t *)addr)[9], ((u8_t *)addr)[10], ((u8_t *)addr)[11], ((u8_t *)addr)[12], ((u8_t *)addr)[13], ((u8_t *)addr)[14], ((u8_t *)addr)[15])
#define PRINTLLADDR(lladdr) PRINTF("[%02x:%02x:%02x:%02x:%02x:%02x]",(lladdr)->addr[0], (lladdr)->addr[1], (lladdr)->addr[2], (lladdr)->addr[3],(lladdr)->addr[4], (lladdr)->addr[5])
#else
#define PRINTF(...)
#define PRINT6ADDR(addr)
#define PRINTLLADDR(addr)
#endif
#endif // #}

MEMB(observers_memb, coap_observer_t, COAP_MAX_OBSERVERS);
LIST(observers_list);

/*-----------------------------------------------------------------------------------*/
coap_observer_t *
coap_add_observer(const char *url, uip_ipaddr_t *addr, uint16_t port, const uint8_t *token, size_t token_len)
{
  coap_observer_t *o = memb_alloc(&observers_memb);

  if (o)
  {
    o->url = url;
    uip_ipaddr_copy(&o->addr, addr);
    o->port = port;
    o->token_len = token_len;
    memcpy(o->token, token, token_len);

    stimer_set(&o->refresh_timer, COAP_OBSERVING_REFRESH_INTERVAL);

    PRINTF("Adding observer for /%s [0x%02X%02X]\n", o->url, o->token[0], o->token[1]);
    list_add(observers_list, o);
  }

  return o;
}
/*-----------------------------------------------------------------------------------*/
void
coap_remove_observer(coap_observer_t *o)
{
  PRINTF("Removing observer for /%s [0x%02X%02X]\n", o->url, o->token[0], o->token[1]);

  memb_free(&observers_memb, o);
  list_remove(observers_list, o);
}

int
coap_remove_observer_by_client(uip_ipaddr_t *addr, uint16_t port)
{
  int removed = 0;
  coap_observer_t* obs = NULL;

  for (obs = (coap_observer_t*)list_head(observers_list); obs; obs = obs->next)
  {
    PRINTF("Remove check client ");
    PRINT6ADDR(addr);
    PRINTF(":%u\n", port);
    if (uip_ipaddr_cmp(&obs->addr, addr) && obs->port==port)
    {
      coap_remove_observer(obs);
      removed++;
    }
  }
  return removed;
}
int
coap_remove_observer_by_token(uip_ipaddr_t *addr, uint16_t port, uint8_t *token, size_t token_len)
{
  int removed = 0;
  coap_observer_t* obs = NULL;

  for (obs = (coap_observer_t*)list_head(observers_list); obs; obs = obs->next)
  {
    PRINTF("Remove check Token 0x%02X%02X\n", token[0], token[1]);
    if (uip_ipaddr_cmp(&obs->addr, addr) && obs->port==port && memcmp(obs->token, token, token_len)==0)
    {
      coap_remove_observer(obs);
      removed++;
    }
  }
  return removed;
}
int
coap_remove_observer_by_url(const char *url)
{
  int removed = 0;
  coap_observer_t* obs = NULL;

  for (obs = (coap_observer_t*)list_head(observers_list); obs; obs = obs->next)
  {
    PRINTF("Remove check URL %p\n", url);
    if (obs->url==url || memcmp(obs->url, url, strlen(obs->url))==0)
    {
      coap_remove_observer(obs);
      removed++;
    }
  }
  return removed;
}
/*-----------------------------------------------------------------------------------*/
void
coap_notify_observers(const char *url, int type, uint32_t observe, uint8_t *payload, size_t payload_len)
{
  coap_observer_t* obs = NULL;
  for (obs = (coap_observer_t*)list_head(observers_list); obs; obs = obs->next)
  {
    if (obs->url==url) /* using RESOURCE url pointer as handle */
    {
      coap_transaction_t *transaction = NULL;

      /*TODO implement special transaction for CON, sharing the same buffer to allow for more observers */

      if ( (transaction = coap_new_transaction(coap_get_tid(), &obs->addr, obs->port)) )
      {
        /* Use CON to check whether client is still there/interested after COAP_OBSERVING_REFRESH_INTERVAL. */
        if (stimer_expired(&obs->refresh_timer))
        {
          PRINTF("Observing: Refresh client with CON\n");
          type = COAP_TYPE_CON;
          stimer_restart(&obs->refresh_timer);
        }

        /* prepare response */
        coap_packet_t push[1]; /* This way the packet can be treated as pointer as usual. */
        coap_init_message(push, (coap_message_type_t)type, CONTENT_2_05, transaction->tid );
        coap_set_header_observe(push, observe);
        coap_set_header_token(push, obs->token, obs->token_len);
        coap_set_payload(push, payload, payload_len);
        transaction->packet_len = coap_serialize_message(push, transaction->packet);

        PRINTF("Observing: Notify from /%s for ", url);
        PRINT6ADDR(&obs->addr);
        PRINTF(":%u\n", obs->port);
        PRINTF("  %.*s\n", payload_len, payload);

        coap_send_transaction(transaction);
      }
    }
  }
}
/*-----------------------------------------------------------------------------------*/
#if 0 // {# don't want to depend on resource_t from erbium
void
coap_observe_handler(resource_t *resource, void *request, void *response)
{
  static char content[26];

  if (response && ((coap_packet_t *)response)->code<128) /* response without error code */
  {
    if (IS_OPTION((coap_packet_t *)request, COAP_OPTION_OBSERVE))
    {
      if (!IS_OPTION((coap_packet_t *)request, COAP_OPTION_TOKEN))
      {
        /* Set default token. */
        coap_set_header_token(request, (uint8_t *)"", 1);
      }

      if (coap_add_observer(resource->url, &UIP_IP_BUF->srcipaddr, UIP_UDP_BUF->srcport, ((coap_packet_t *)request)->token, ((coap_packet_t *)request)->token_len))
      {
        coap_set_header_observe(response, 0);
        coap_set_payload(response, (uint8_t *)content, snprintf(content, sizeof(content), "Added as observer %u/%u", list_length(observers_list), COAP_MAX_OBSERVERS));
      }
      else
      {
        ((coap_packet_t *)response)->code = SERVICE_UNAVAILABLE_5_03;
        coap_set_payload(response, (uint8_t *)"Too many observers", 18);
      } /* if (added observer) */
    }
    else /* if (observe) */
    {
      /* Remove client if it is currently observing. */
      coap_remove_observer_by_client(&UIP_IP_BUF->srcipaddr, UIP_UDP_BUF->srcport);
    } /* if (observe) */
  }
}
#endif // #}
