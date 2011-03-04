#ifndef EINIKUEDOOJAVAICHOOP
#define EINIKUEDOOJAVAICHOOP

#include "contiki.h"

// char* serial_receive();

typedef struct {
    void* ec_cont_frame;
    void* ec_cont_label;
    char* ec_result;
// internal
    int waiting;
} frame_serial_receive;

void serial_receive(frame_serial_receive*);

// void sleep(int);
typedef struct {
    void* ec_cont_frame;
    void* ec_cont_label;
// internal
    struct etimer et;
} frame_sleep;

void timer_sleep(frame_sleep*, int seconds);

int get_threadid();

#endif
