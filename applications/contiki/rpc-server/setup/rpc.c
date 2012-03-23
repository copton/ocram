#include "rpc.h"
#include "debug.h"
#include "net/uip-debug.h"
#include <string.h>
#include <stdio.h>

// RPC call
uint8_t sequence = 0;

void rpc_call_tell(uip_ipaddr_t* peer, rpc_call_t* remote_call, rpc_call_t* call)
{
    call->function = RPC_TELL;
    call->sequence = sequence++;
    call->data.tell.peer = *peer;
    call->data.tell.call = remote_call;
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
    
    if (offset + 1 > len) { return -1; }
    buffer[offset++] = call->sequence;

    if (offset + 1 > len) { return -1; }
    buffer[offset++] = call->function;

    if (0) {
    } else if (call->function == RPC_TELL) {
        if (copy(call->data.tell.peer.u8, sizeof(call->data.tell.peer.u8), buffer, len, &offset)) {
            return -1;
        }

        int16_t size;
        size = rpc_marshall_call(call->data.tell.call, buffer + offset, len - offset);
        if (size == -1) { return -1; }
        offset += size;
    } else if (call->function == RPC_READ_SLOW_SENSOR) {
        if (copy(&call->data.read_slow_sensor.sensor, sizeof(call->data.read_slow_sensor.sensor), buffer, len, &offset)) {
            return -1;
        }
    } else if (call->function == RPC_READ_FAST_SENSOR) {
        if (copy(&call->data.read_slow_sensor.sensor, sizeof(call->data.read_slow_sensor.sensor), buffer, len, &offset)) {
            return -1;
        }
    } else {
        return -1;
    } 
    return offset;
}

bool rpc_unmarshall_call(rpc_call_t* call, size_t numberofCalls, uint8_t* buffer, size_t len)
{
    if (numberofCalls == 0) {return false;}

    size_t offset = 0;

    if (offset + 1 > len) {return false;}
    call->sequence = buffer[offset++]; 

    if (offset + 1 > len) {return false;}
    call->function = buffer[offset++]; 

    if (0) {
    } else if (call->function == RPC_TELL) {
        if (rcopy(call->data.tell.peer.u8, sizeof(call->data.tell.peer.u8), buffer, len, &offset)) {
            return false;
        }

        if (! rpc_unmarshall_call(call + 1, numberofCalls - 1, buffer + offset, len - offset)) {
            return false;
        }
        call->data.tell.call = call + 1;
    } else if (call->function == RPC_READ_SLOW_SENSOR) {
        if (rcopy(&call->data.read_slow_sensor.sensor, sizeof(call->data.read_slow_sensor.sensor), buffer, len, &offset)) {
            return false;
        }
    } else if (call->function == RPC_READ_FAST_SENSOR) {
        if (rcopy(&call->data.read_fast_sensor.sensor, sizeof(call->data.read_fast_sensor.sensor), buffer, len, &offset)) {
            return false;
        }
    } else {
        return false;
    }

    return true;
}

const char* rpc_function_to_string(rpc_function_t function) {
    switch (function) {
        case RPC_TELL: return "RPC_TELL";
        case RPC_READ_FAST_SENSOR: return "RPC_READ_FAST_SENSOR";
        case RPC_READ_SLOW_SENSOR: return "RPC_READ_SLOW_SENSOR";
        default: return "UNKNOWN";
    }
}

static void print_call(rpc_call_t* call) {
    printf("\"type\":\"call\", \"seq\":%d, \"fun\":\"%s\", ", call->sequence, rpc_function_to_string(call->function));
    if (0) {
    } else if (call->function == RPC_TELL) {
        printf("\"peer\":\""); uip_debug_ipaddr_print(&call->data.tell.peer); printf("\", \"call\":{");
        print_call(call->data.tell.call);
        printf("}");
    } else if (call->function == RPC_READ_SLOW_SENSOR) {
        printf("\"sensor\":%d", call->data.read_slow_sensor.sensor);
    } else if (call->function == RPC_READ_FAST_SENSOR) {
        printf("\"sensor\":%d", call->data.read_fast_sensor.sensor);
    } else {
        ASSERT(false);
    }
}

void rpc_print_call(rpc_call_t* call) {
    printf("{");
    print_call(call);
    printf("}\n");
}
void rpc_response_tell(rpc_response_t* remote_response, rpc_call_t* call, rpc_response_t* response)
{
    ASSERT(call->function == RPC_TELL);
    response->function = call->function;
    response->sequence = call->sequence;
    response->data.tell.response = remote_response;
}

void rpc_response_read_slow_sensor(int16_t value, rpc_call_t* call, rpc_response_t* response)
{
    ASSERT(call->function == RPC_READ_SLOW_SENSOR);
    response->function = call->function;
    response->sequence = call->sequence;
    response->data.read_slow_sensor.value = value;
}

void rpc_response_read_fast_sensor(int16_t value, rpc_call_t* call, rpc_response_t* response)
{
    ASSERT(call->function == RPC_READ_FAST_SENSOR);
    response->function = call->function;
    response->sequence = call->sequence;
    response->data.read_fast_sensor.value = value;
}

int16_t rpc_marshall_response(rpc_response_t* response, uint8_t* buffer, size_t len)
{
    int16_t offset = 0;
    
    if (offset + 1 > len) { debug_mark = 20; return -1; }
    buffer[offset++] = response->sequence;

    if (offset + 1 > len) { debug_mark = 21; return -1; }
    buffer[offset++] = response->function;

    if (0) {
    } else if (response->function == RPC_TELL) {
        size_t size = rpc_marshall_response(response->data.tell.response, buffer + offset, len - offset);
        if (size == -1) { debug_mark = 22; return -1; }
        offset += size;
    } else if (response->function == RPC_READ_SLOW_SENSOR) {
        if (copy(&response->data.read_slow_sensor.value, sizeof(response->data.read_slow_sensor.value), buffer, len, &offset)) { debug_mark = 23; return -1; }
    } else if (response->function == RPC_READ_FAST_SENSOR) {
        if (copy(&response->data.read_slow_sensor.value, sizeof(response->data.read_slow_sensor.value), buffer, len, &offset)) { debug_mark = 24; return -1; }
    } else {
        debug_mark = 25; return -1;
    }
    return offset;
}

bool rpc_unmarshall_response(rpc_response_t* response, size_t numberofResponses, uint8_t* buffer, size_t len)
{
    if (numberofResponses == 0) return false;

    size_t offset = 0;

    if (offset + 1 > len) return false;
    response->sequence = buffer[offset++]; 
    
    if (offset + 1 > len) return false;
    response->function = buffer[offset++]; 

    if (0) {
    } else if (response->function == RPC_TELL) {
        if (! rpc_unmarshall_response(response + 1, numberofResponses - 1, buffer + offset, len - offset)) {
            return false;
        }
        response->data.tell.response = response + 1;
    } else if (response->function == RPC_READ_SLOW_SENSOR) {
        if (rcopy(&response->data.read_slow_sensor.value, sizeof(response->data.read_slow_sensor.value), buffer, len, &offset)) return false;
    } else if (response->function == RPC_READ_FAST_SENSOR) {
        if (rcopy(&response->data.read_fast_sensor.value, sizeof(response->data.read_fast_sensor.value), buffer, len, &offset)) return false;
    } else {
        return false;
    }

    return true;
}

static void print_response(rpc_response_t* response)
{
    printf("\"type\":\"response\", \"seq\":%d, \"fun\":\"%s\", ", response->sequence, rpc_function_to_string(response->function));
    if (0) {
    } else if (response->function == RPC_TELL) {
        printf("\"call\":{");
        print_response(response->data.tell.response);
        printf("}");
    } else if (response->function == RPC_READ_SLOW_SENSOR) {
        printf("\"value\":%d", response->data.read_slow_sensor.value);
    } else if (response->function == RPC_READ_FAST_SENSOR) {
        printf("\"value\":%d", response->data.read_fast_sensor.value);
    } else {
        ASSERT(false);
    }
}

void rpc_print_response(rpc_response_t* response)
{
    printf("{");
    print_response(response);
    printf("}\n");
}
