#ifndef XEPHIUCAHPHOOSHAEJOH
#define XEPHIUCAHPHOOSHAEJOH

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

typedef enum {
  SUCCESS        =  0,
  FAIL           =  1
} error_t;

typedef enum {
    READ = 1,
    WRITE = 2,
    READ_WRITE = 3
} Mode;

typedef uint32_t sensor_val_t;

#endif
