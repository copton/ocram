#include "rpc.h"
#include "debug.h"
#include <string.h>

// RPC call
uint8_t sequence = 0;

void rpc_call_tell(uip_ipaddr_t* peer, rpc_call_t* remote_call, rpc_call_t* call)
{
    call->function = RPC_TELL;
    call->sequence = sequence++;
    call->data.tell.client.rpc_call = remote_call;
    call->data.tell.client.peer = *peer;
}

void rpc_call_read_slow_sensor(int sensor, rpc_call_t* call)
{
    call->function = RPC_READ_SLOW_SENSOR;
    call->sequence = sequence++;
    call->data.read_slow_sensor.sensor = sensor;
}

void rpc_call_read_fast_sensor(int sensor, rpc_call_t* call) {
    call->function = RPC_READ_FAST_SENSOR;
    call->sequence = sequence++;
    call->data.read_fast_sensor.sensor = sensor;
}

static inline bool copy(void* source, size_t n, uint8_t* buffer, size_t len, int16_t* offset)
{
    if (*offset + n > len) return true;
    memcpy(buffer + *offset, source, n);
    *offset += n;
    return false;
}

static inline bool rcopy(void* destination, size_t n, uint8_t* buffer, size_t len, int16_t* offset)
{
    if (*offset + n > len) return true;
    memcpy(destination, buffer + *offset, n);
    *offset += n;
    return false;
}

int16_t rpc_marshall_call(rpc_call_t* call, uint8_t* buffer, size_t len)
{
    int16_t offset = 0;
    
    if (offset + 1 > len) return -1;
    buffer[offset++] = call->sequence;

    if (offset + 1 > len) return -1;
    buffer[offset++] = call->function;

    switch (call->function) {
        case RPC_TELL: {
            if (copy(call->data.tell.client.peer.u8, sizeof(call->data.tell.client.peer.u8), buffer, len, &offset)) return -1;
            if (copy(call->data.tell.client.peer.u16, sizeof(call->data.tell.client.peer.u16), buffer, len, &offset)) return -1;

            int16_t size;
            if ((size = rpc_marshall_call(call->data.tell.client.rpc_call, buffer + offset, len - offset)) == -1) return -1;
            offset += size;
        } break;
        case RPC_READ_SLOW_SENSOR: {
            if (copy(&call->data.read_slow_sensor.sensor, sizeof(call->data.read_slow_sensor.sensor), buffer, len, &offset)) return -1;
        } break;
        case RPC_READ_FAST_SENSOR: {
            if (copy(&call->data.read_slow_sensor.sensor, sizeof(call->data.read_slow_sensor.sensor), buffer, len, &offset)) return -1;
        } break;
    }
    return offset;
}

bool rpc_unmarshall_call(rpc_call_t* call, uint8_t* buffer, size_t len)
{
    size_t offset = 0;

    if (offset + 1 > len) return false;
    call->sequence = buffer[offset++]; 

    if (offset + 1 > len) return false;
    call->function = buffer[offset++]; 

    switch (call->function) {
        case RPC_TELL: {
            if (rcopy(call->data.tell.server.peer.u8, sizeof(call->data.tell.server.peer.u8), buffer, len, &offset)) return false;
            if (rcopy(call->data.tell.server.peer.u16, sizeof(call->data.tell.server.peer.u16), buffer, len, &offset)) return false;

            call->data.tell.server.buffer = buffer + offset;
            call->data.tell.server.len = len - offset;
        } break;
        case RPC_READ_SLOW_SENSOR: {
            if (rcopy(&call->data.read_slow_sensor.sensor, sizeof(call->data.read_slow_sensor.sensor), buffer, len, &offset)) return false;
        }
        case RPC_READ_FAST_SENSOR: {
            if (rcopy(&call->data.read_fast_sensor.sensor, sizeof(call->data.read_fast_sensor.sensor), buffer, len, &offset)) return false;
        }
    }

    return true;
}

void rpc_responde_tell(rpc_call_t* call, rpc_response_t* response)
{
    ASSERT(call->function == RPC_TELL);
    response->function = call->function;
    response->sequence = call->sequence;
}

void rpc_reponsde_read_slow_sensor(int16_t value, rpc_call_t* call, rpc_response_t* response)
{
    ASSERT(call->function == RPC_READ_SLOW_SENSOR);
    response->function = call->function;
    response->sequence = call->sequence;
    response->data.read_slow_sensor.value = value;
}

void rpc_reponsde_read_fast_sensor(int16_t value, rpc_call_t* call, rpc_response_t* response)
{
    ASSERT(call->function == RPC_READ_FAST_SENSOR);
    response->function = call->function;
    response->sequence = call->sequence;
    response->data.read_fast_sensor.value = value;
}

int16_t rpc_marshall_response(rpc_response_t* response, uint8_t* buffer, size_t len)
{
    int16_t offset = 0;
    
    if (offset + 1 > len) return -1;
    buffer[offset++] = response->sequence;

    if (offset + 1 > len) return -1;
    buffer[offset++] = response->function;

    switch (response->function) {
        case RPC_TELL: {
        } break;
        case RPC_READ_SLOW_SENSOR: {
            if (copy(&response->data.read_slow_sensor.value, sizeof(response->data.read_slow_sensor.value), buffer, len, &offset)) return -1;
        } break;
        case RPC_READ_FAST_SENSOR: {
            if (copy(&response->data.read_slow_sensor.value, sizeof(response->data.read_slow_sensor.value), buffer, len, &offset)) return -1;
        } break;
    }
    return offset;
}

bool rpc_unmarshall_response(rpc_response_t* response, uint8_t* buffer, size_t len)
{
    size_t offset = 0;

    if (offset + 1 > len) return false;
    response->sequence = buffer[offset++]; 
    
    if (offset + 1 > len) return false;
    response->function = buffer[offset++]; 

    switch (response->function) {
        case RPC_TELL: {
        } break;
        case RPC_READ_SLOW_SENSOR: {
            if (rcopy(&response->data.read_slow_sensor.value, sizeof(response->data.read_slow_sensor.value), buffer, len, &offset)) return false;
        }
        case RPC_READ_FAST_SENSOR: {
            if (rcopy(&response->data.read_fast_sensor.value, sizeof(response->data.read_fast_sensor.value), buffer, len, &offset)) return false;
        }
    }

    return true;
}
