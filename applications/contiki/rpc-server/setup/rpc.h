#ifndef ZIESHEILIENGOHNOHCHU
#define ZIESHEILIENGOHNOHCHU

#include "net/uip.h"
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

typedef enum {
    RPC_TELL = 1,         // recursion
    RPC_READ_SLOW_SENSOR, // blocking 
    RPC_READ_FAST_SENSOR  // non-blocking
} rpc_function_t;

struct rpc_call {
    uint8_t sequence;
    rpc_function_t function;
    union {
        union {
            struct {
                uip_ipaddr_t peer;
                struct rpc_call* rpc_call;
            } client;
            struct {
                uip_ipaddr_t peer;
                uint8_t* buffer;
                size_t len;
            } server;
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
bool rpc_unmarshall_call(rpc_call_t* call, uint8_t* buffer, size_t len);


typedef struct {
    uint8_t sequence;
    rpc_function_t function;
    union {
        struct {
            uint16_t value;
        } read_slow_sensor;
        struct {
            uint16_t value;
        } read_fast_sensor;
    } data;
} rpc_response_t;

void rpc_responde_tell(rpc_call_t* call, rpc_response_t* response);
void rpc_reponsde_read_slow_sensor(int16_t value, rpc_call_t* call, rpc_response_t* response);
void rpc_reponsde_read_fast_sensor(int16_t value, rpc_call_t* call, rpc_response_t* response);
int16_t rpc_marshall_response(rpc_response_t* response, uint8_t* buffer, size_t len);
bool rpc_unmarshall_response(rpc_response_t* call, uint8_t* buffer, size_t len);

#endif
