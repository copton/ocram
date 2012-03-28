#ifndef ZIESHEILIENGOHNOHCHU
#define ZIESHEILIENGOHNOHCHU

#include "net/uip.h"
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

typedef enum {
    RPC_TELL = 1,         // recursion
    RPC_READ_SLOW_SENSOR = 2, // blocking 
    RPC_READ_FAST_SENSOR = 3,  // non-blocking
} rpc_function_t;

const char* rpc_function_to_string(rpc_function_t function);

struct rpc_call {
    uint8_t sequence;
    rpc_function_t function;
    union {
        struct {
            uip_ipaddr_t peer;
            struct rpc_call* call;
        } tell;
        struct {
            int sensor;
        } read_slow_sensor;
        struct {
            int sensor;
        } read_fast_sensor;
    } data;
};
typedef struct rpc_call rpc_call_t;

void rpc_call_tell(uip_ipaddr_t* peer, rpc_call_t* remote_call, rpc_call_t* call);
void rpc_call_read_slow_sensor(int sensor, rpc_call_t* call);
void rpc_call_read_fast_sensor(int sensor, rpc_call_t* call);
int16_t rpc_marshall_call(rpc_call_t* call, uint8_t* buffer, size_t len);
bool rpc_unmarshall_call(rpc_call_t* call, size_t numberofCalls, uint8_t* buffer, size_t len);
void rpc_print_call(rpc_call_t* call);


struct rpc_response {
    uint8_t sequence;
    rpc_function_t function;
    union {
        struct {
            struct rpc_response* response;
        } tell;
        struct {
            uint16_t value;
        } read_slow_sensor;
        struct {
            uint16_t value;
        } read_fast_sensor;
    } data;
};
typedef struct rpc_response rpc_response_t;

void rpc_response_tell(rpc_response_t* remote_response, rpc_call_t* call, rpc_response_t* response);
void rpc_response_read_slow_sensor(int16_t value, rpc_call_t* call, rpc_response_t* response);
void rpc_response_read_fast_sensor(int16_t value, rpc_call_t* call, rpc_response_t* response);
int16_t rpc_marshall_response(rpc_response_t* response, uint8_t* buffer, size_t len);
bool rpc_unmarshall_response(rpc_response_t* call, size_t numberofResponses, uint8_t* buffer, size_t len);
void rpc_print_response(rpc_response_t* response);

#endif
