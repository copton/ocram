#ifndef SIEZOTOHCHIONGAIDAEK
#define SIEZOTOHCHIONGAIDAEK

#include "process.h"
#include <stdbool.h>

typedef struct {
    struct process *waiting_process;
    bool waiting;
} condition_t;

#endif
