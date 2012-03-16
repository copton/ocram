#ifndef SIEZOTOHCHIONGAIDAEK
#define SIEZOTOHCHIONGAIDAEK

#include "process.h"
#include <stdbool.h>

typedef struct {
    struct process *waiting_process;
    bool waiting;
} condition_t;

#define TC_CONDITION_INITIALIZER {NULL, false}

void tc_condition_init(condition_t* cond);

#endif
