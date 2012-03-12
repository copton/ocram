#ifndef COAP
#define COAP

// {# er-coap-07.h #}
#include <stddef.h> /* for size_t */
#include "contiki-net.h"
// {#
// #include "erbium.h"
typedef void (*restful_response_handler)(void*, void*);
typedef int (* service_callback_t)(void *request, void *response, uint8_t *buffer, uint16_t preferred_size, int32_t *offset);
// #}

#define COAP_DEFAULT_PORT                    5683

#ifndef COAP_SERVER_PORT
#define COAP_SERVER_PORT                     COAP_DEFAULT_PORT
#endif

#define COAP_DEFAULT_MAX_AGE                 60
#define COAP_RESPONSE_TIMEOUT                2
#define COAP_RESPONSE_RANDOM_FACTOR          1.5
#define COAP_MAX_RETRANSMIT                  4

#define COAP_HEADER_LEN                      4 /* | oc:0xF0 type:0x0C version:0x03 | code | tid:0x00FF | tid:0xFF00 | */
#define COAP_ETAG_LEN                        8 /* The maximum number of bytes for the ETag */
#define COAP_TOKEN_LEN                       8 /* The maximum number of bytes for the Token */
#define COAP_MAX_ACCEPT_NUM                  2 /* The maximum number of accept preferences to parse/store */

#define COAP_HEADER_VERSION_MASK             0xC0
#define COAP_HEADER_VERSION_POSITION         6
#define COAP_HEADER_TYPE_MASK                0x30
#define COAP_HEADER_TYPE_POSITION            4
#define COAP_HEADER_OPTION_COUNT_MASK        0x0F
#define COAP_HEADER_OPTION_COUNT_POSITION    0

#define COAP_HEADER_OPTION_DELTA_MASK        0xF0
#define COAP_HEADER_OPTION_SHORT_LENGTH_MASK 0x0F

/*
 * Conservative size limit, as not all options have to be set at the same time.
 */
/*                            Hdr CoT Age  Tag              Obs  Tok               Blo strings */
#define COAP_MAX_HEADER_SIZE  (4 + 3 + 5 + 1+COAP_ETAG_LEN + 3 + 1+COAP_TOKEN_LEN + 4 + 10) /* 50 */
#define COAP_MAX_PACKET_SIZE  (COAP_MAX_HEADER_SIZE + REST_MAX_CHUNK_SIZE)
/*                                        0/14          48 for IPv6 (28 for IPv4) */
#if COAP_MAX_PACKET_SIZE > (UIP_BUFSIZE - UIP_LLH_LEN - UIP_IPUDPH_LEN)
#error "UIP_CONF_BUFFER_SIZE too small for REST_MAX_CHUNK_SIZE"
#endif

/*
 * Maximum number of failed request attempts before action
 */
#ifndef COAP_MAX_ATTEMPTS
#define COAP_MAX_ATTEMPTS             4
#endif /* COAP_MAX_ATTEMPTS */

#define UIP_IP_BUF   ((struct uip_ip_hdr *)&uip_buf[UIP_LLH_LEN])
#define UIP_UDP_BUF  ((struct uip_udp_hdr *)&uip_buf[uip_l2_l3_hdr_len])

#define SET_OPTION(packet, opt) ((packet)->options |= 1L<<opt)
#define IS_OPTION(packet, opt) ((packet)->options & 1L<<opt)

#ifndef MIN
#define MIN(a, b) ((a) < (b)? (a) : (b))
#endif /* MIN */

/* CoAP message types */
typedef enum {
  COAP_TYPE_CON, /* confirmables */
  COAP_TYPE_NON, /* non-confirmables */
  COAP_TYPE_ACK, /* acknowledgements */
  COAP_TYPE_RST  /* reset */
} coap_message_type_t;

/* CoAP request method codes */
typedef enum {
  COAP_GET = 1,
  COAP_POST,
  COAP_PUT,
  COAP_DELETE
} coap_method_t;

/* CoAP response codes */
typedef enum {
  NO_ERROR = 0,

  CREATED_2_01 = 65,                    /* CREATED */
  DELETED_2_02 = 66,                    /* DELETED */
  VALID_2_03 = 67,                      /* NOT_MODIFIED */
  CHANGED_2_04 = 68,                    /* CHANGED */
  CONTENT_2_05 = 69,                    /* OK */

  BAD_REQUEST_4_00 = 128,               /* BAD_REQUEST */
  UNAUTHORIZED_4_01 = 129,              /* UNAUTHORIZED */
  BAD_OPTION_4_02 = 130,                /* BAD_OPTION */
  FORBIDDEN_4_03 = 131,                 /* FORBIDDEN */
  NOT_FOUND_4_04 = 132,                 /* NOT_FOUND */
  METHOD_NOT_ALLOWED_4_05 = 133,        /* METHOD_NOT_ALLOWED */
  PRECONDITION_FAILED_4_12 = 140,       /* BAD_REQUEST */
  REQUEST_ENTITY_TOO_LARGE_4_13 = 141,  /* REQUEST_ENTITY_TOO_LARGE */
  UNSUPPORTED_MADIA_TYPE_4_15 = 143,    /* UNSUPPORTED_MADIA_TYPE */

  INTERNAL_SERVER_ERROR_5_00 = 160,     /* INTERNAL_SERVER_ERROR */
  NOT_IMPLEMENTED_5_01 = 161,           /* NOT_IMPLEMENTED */
  BAD_GATEWAY_5_02 = 162,               /* BAD_GATEWAY */
  SERVICE_UNAVAILABLE_5_03 = 163,       /* SERVICE_UNAVAILABLE */
  GATEWAY_TIMEOUT_5_04 = 164,           /* GATEWAY_TIMEOUT */
  PROXYING_NOT_SUPPORTED_5_05 = 165,    /* PROXYING_NOT_SUPPORTED */

  /* Memory errors */
  IMPLEMENTATION_ERROR = 192,
  MEMORY_ALLOC_ERR = 193,
  MEMORY_BOUNDARY_EXCEEDED = 194,

  /* CoAP errors */
  UNIMPLEMENTED_CRITICAL_OPTION,
  UNKNOWN_CRITICAL_OPTION,
  PACKET_SERIALIZATION_ERROR
} coap_status_t;

/* CoAP header options */
typedef enum {
  COAP_OPTION_CONTENT_TYPE = 1,   /* 0-2 B */
  COAP_OPTION_MAX_AGE = 2,        /* 0-4 B */
  COAP_OPTION_PROXY_URI = 3,      /* 1-270 B */
  COAP_OPTION_ETAG = 4,           /* 1-8 B */
  COAP_OPTION_URI_HOST = 5,       /* 1-270 B */
  COAP_OPTION_LOCATION_PATH = 6,  /* 1-270 B */
  COAP_OPTION_URI_PORT = 7,       /* 0-2 B */
  COAP_OPTION_LOCATION_QUERY = 8, /* 1-270 B */
  COAP_OPTION_URI_PATH = 9,       /* 1-270 B */
    COAP_OPTION_OBSERVE = 10,       /* 0-2 B */
  COAP_OPTION_TOKEN = 11,         /* 1-8 B */
  COAP_OPTION_ACCEPT = 12,        /* 0-2 B */
  COAP_OPTION_IF_MATCH = 13,      /* 0-8 B */
  COAP_OPTION_FENCE_POST = 14,    /* 0 B */
  COAP_OPTION_URI_QUERY = 15,     /* 1-270 B */
    COAP_OPTION_BLOCK2 = 17,        /* 1-3 B */
    COAP_OPTION_BLOCK1 = 19,        /* 1-3 B */
  COAP_OPTION_IF_NONE_MATCH = 21  /* 0 B */
} coap_option_t;

/* CoAP content-types */
typedef enum {
  TEXT_PLAIN = 0,
    TEXT_XML = 1, /* Intented types are not in the initial registry. */
    TEXT_CSV = 2,
    TEXT_HTML = 3,
    IMAGE_GIF = 21,
    IMAGE_JPEG = 22,
    IMAGE_PNG = 23,
    IMAGE_TIFF = 24,
    AUDIO_RAW = 25,
    VIDEO_RAW = 26,
  APPLICATION_LINK_FORMAT = 40, /* Actually not in registry!? */
  APPLICATION_XML = 41,
  APPLICATION_OCTET_STREAM = 42,
    APPLICATION_RDF_XML = 43,
    APPLICATION_SOAP_XML = 44,
    APPLICATION_ATOM_XML = 45,
    APPLICATION_XMPP_XML = 46,
  APPLICATION_EXI = 47,
    APPLICATION_FASTINFOSET = 48,
    APPLICATION_SOAP_FASTINFOSET = 49,
  APPLICATION_JSON = 50,
    APPLICATION_X_OBIX_BINARY = 51
} coap_content_type_t;

typedef struct {
  uint8_t *buffer; /* pointer to CoAP header / incoming packet buffer / memory to serialize packet */

  uint8_t version;
  coap_message_type_t type;
  uint8_t option_count;
  uint8_t code;
  uint16_t tid;

  uint32_t options;  /* Bitmap to check if option is set */

  coap_content_type_t content_type; /* Parse options once and store; allows setting options in random order  */
  uint32_t max_age;
  size_t proxy_uri_len;
  char *proxy_uri;
  uint8_t etag_len;
  uint8_t etag[COAP_ETAG_LEN];
  size_t uri_host_len;
  char *uri_host;
  size_t location_path_len;
  char *location_path;
  uint16_t uri_port;
  size_t location_query_len;
  char *location_query;
  size_t uri_path_len;
  char *uri_path;
  uint16_t observe;
  uint8_t token_len;
  uint8_t token[COAP_TOKEN_LEN];
  uint8_t accept_num;
  uint16_t accept[COAP_MAX_ACCEPT_NUM];
  uint8_t if_match_len;
  uint8_t if_match[COAP_ETAG_LEN];
  uint32_t block2_num;
  uint8_t block2_more;
  uint16_t block2_size;
  uint32_t block2_offset;
  uint32_t block1_num;
  uint8_t block1_more;
  uint16_t block1_size;
  uint32_t block1_offset;
  size_t uri_query_len;
  char *uri_query;
  uint8_t if_none_match;

  uint16_t payload_len;
  uint8_t *payload;

} coap_packet_t;


/* To store error code and human-readable payload */
extern coap_status_t coap_error_code;
extern char *coap_error_message;

void coap_init_connection(uint16_t port);
uint16_t coap_get_tid(void);

void coap_init_message(void *packet, coap_message_type_t type, uint8_t code, uint16_t tid);
size_t coap_serialize_message(void *packet, uint8_t *buffer);
void coap_send_message(uip_ipaddr_t *addr, uint16_t port, uint8_t *data, uint16_t length);
coap_status_t coap_parse_message(void *request, uint8_t *data, uint16_t data_len);

int coap_get_query_variable(void *packet, const char *name, const char **output);
int coap_get_post_variable(void *packet, const char *name, const char **output);

unsigned int coap_get_header_content_type(void *packet);
int coap_set_header_content_type(void *packet, unsigned int content_type);

int coap_get_header_accept(void *packet, uint16_t **accept);
int coap_set_header_accept(void *packet, uint16_t accept);

int coap_get_header_max_age(void *packet, uint32_t *age);
int coap_set_header_max_age(void *packet, uint32_t age);

int coap_get_header_etag(void *packet, const uint8_t **etag);
int coap_set_header_etag(void *packet, uint8_t *etag, size_t etag_len);

int coap_get_header_if_match(void *packet, const uint8_t **etag);
int coap_set_header_if_match(void *packet, uint8_t *etag, size_t etag_len);

int coap_get_header_if_none_match(void *packet);
int coap_set_header_if_none_match(void *packet);

int coap_get_header_token(void *packet, const uint8_t **token);
int coap_set_header_token(void *packet, uint8_t *token, size_t token_len);

int coap_get_header_proxy_uri(void *packet, const char **uri); /* In-place string might not be 0-terminated. */
int coap_set_header_proxy_uri(void *packet, char *uri);

int coap_get_header_uri_host(void *packet, const char **host); /* In-place string might not be 0-terminated. */
int coap_set_header_uri_host(void *packet, char *host);

int coap_get_header_uri_path(void *packet, const char **path); /* In-place string might not be 0-terminated. */
int coap_set_header_uri_path(void *packet, char *path);

int coap_get_header_uri_query(void *packet, const char **query); /* In-place string might not be 0-terminated. */
int coap_set_header_uri_query(void *packet, char *query);

int coap_get_header_location_path(void *packet, const char **path); /* In-place string might not be 0-terminated. */
int coap_set_header_location_path(void *packet, char *path); /* Also splits optional query into Location-Query option. */

int coap_get_header_location_query(void *packet, const char **query); /* In-place string might not be 0-terminated. */
int coap_set_header_location_query(void *packet, char *query);

int coap_get_header_observe(void *packet, uint32_t *observe);
int coap_set_header_observe(void *packet, uint32_t observe);

int coap_get_header_block2(void *packet, uint32_t *num, uint8_t *more, uint16_t *size, uint32_t *offset);
int coap_set_header_block2(void *packet, uint32_t num, uint8_t more, uint16_t size);

int coap_get_header_block1(void *packet, uint32_t *num, uint8_t *more, uint16_t *size, uint32_t *offset);
int coap_set_header_block1(void *packet, uint32_t num, uint8_t more, uint16_t size);

int coap_get_payload(void *packet, const uint8_t **payload);
int coap_set_payload(void *packet, uint8_t *payload, size_t length);

// {# er-coap-transactions.h #}
//#include "er-coap-07.h" {##}

/*
 * The number of concurrent messages that can be stored for retransmission in the transaction layer.
 */
#ifndef COAP_MAX_OPEN_TRANSACTIONS
#define COAP_MAX_OPEN_TRANSACTIONS 4 
#endif /* COAP_MAX_OPEN_TRANSACTIONS */


/* container for transactions with message buffer and retransmission info */
typedef struct coap_transaction {
  struct coap_transaction *next; /* for LIST */

  uint16_t tid;
  struct etimer retrans_timer;
  uint8_t retrans_counter;

  uip_ipaddr_t addr;
  uint16_t port;

  restful_response_handler callback;
  void *callback_data;

  uint16_t packet_len;
  uint8_t packet[COAP_MAX_PACKET_SIZE+1]; /* +1 for the terminating '\0' to simply and savely use snprintf(buf, len+1, "", ...) in the resource handler. */
} coap_transaction_t;

// void coap_register_as_transaction_handler(); {# moved to main #}

coap_transaction_t *coap_new_transaction(uint16_t tid, uip_ipaddr_t *addr, uint16_t port);
void coap_send_transaction(coap_transaction_t *t);
void coap_clear_transaction(coap_transaction_t *t);
coap_transaction_t *coap_get_transaction_by_tid(uint16_t tid);

void coap_check_transactions();

// {# er-coap-07-engine.h #}
#if !defined(REST)
#error "Define REST to \"coap_rest_implementation\""
#endif

//#include "er-coap-07.h" {##}
//#include "er-coap-07-transactions.h" {##}
//#include "er-coap-07-observing.h" {# maybe still needed #}
//#include "er-coap-07-separate.h" {# maybe still needed #}

#include "pt.h"

/* Declare server process */
// PROCESS_NAME(coap_receiver); {# moved to app #}

#define SERVER_LISTEN_PORT      UIP_HTONS(COAP_SERVER_PORT)

typedef coap_packet_t rest_request_t;
typedef coap_packet_t rest_response_t;

extern const struct rest_implementation coap_rest_implementation;

// void coap_receiver_init(void); {# moved to app #}
int handle_incoming_data(); // {# needed from app #}

#if 0 // {# moved to app
/*-----------------------------------------------------------------------------------*/
/*- Client part ---------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
struct request_state_t {
    struct pt pt;
    struct process *process;
    coap_transaction_t *transaction;
    coap_packet_t *response;
    uint32_t block_num;
};

typedef void (*blocking_response_handler) (void* response);

PT_THREAD(coap_blocking_request(struct request_state_t *state, process_event_t ev,
                                uip_ipaddr_t *remote_ipaddr, uint16_t remote_port,
                                coap_packet_t *request,
                                blocking_response_handler request_callback));

#define COAP_BLOCKING_REQUEST(server_addr, server_port, request, chunk_handler) \
static struct request_state_t request_state; \
PT_SPAWN(process_pt, &request_state.pt, \
             coap_blocking_request(&request_state, ev, \
                                   server_addr, server_port, \
                                   request, chunk_handler) \
    );
/*-----------------------------------------------------------------------------------*/
#endif // #}

// {# er-coap-07-observing.h #}

// #{
//#include "er-coap-07.h"
//#include "er-coap-07-transactions.h"
// #}

#ifndef COAP_MAX_OBSERVERS
#define COAP_MAX_OBSERVERS      4
#endif /* COAP_MAX_OBSERVERS */

/* Interval in seconds in which NON notifies are changed to CON notifies to check client. */
#define COAP_OBSERVING_REFRESH_INTERVAL  60

#if COAP_MAX_OPEN_TRANSACTIONS<COAP_MAX_OBSERVERS
#warning "COAP_MAX_OPEN_TRANSACTIONS smaller than COAP_MAX_OBSERVERS: cannot handle CON notifications"
#endif

typedef struct coap_observer {
  struct coap_observer *next; /* for LIST */

  const char *url;
  uip_ipaddr_t addr;
  uint16_t port;
  uint8_t token_len;
  uint8_t token[COAP_TOKEN_LEN];
  struct stimer refresh_timer;
} coap_observer_t;

list_t coap_get_observers(void);

coap_observer_t *coap_add_observer(const char *url, uip_ipaddr_t *addr, uint16_t port, const uint8_t *token, size_t token_len);
void coap_remove_observer(coap_observer_t *o);

int coap_remove_observer_by_client(uip_ipaddr_t *addr, uint16_t port);
int coap_remove_observer_by_token(uip_ipaddr_t *addr, uint16_t port, uint8_t *token, size_t token_len);
int coap_remove_observer_by_url(const char *url);

void coap_notify_observers(const char *url, int type, uint32_t observe, uint8_t *payload, size_t payload_len);

// void coap_observe_handler(resource_t *resource, void *request, void *response); {# don't want to depend on resource_t from erbium #}

#endif
