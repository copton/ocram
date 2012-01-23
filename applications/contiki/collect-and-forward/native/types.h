#ifndef TOHTHAEPHAAGHIGHOCIV
#define TOHTHAEPHAAGHIGHOCIV

#include <stdint.h>
#include <stddef.h>

typedef enum {
  SUCCESS        =  0,
  FAIL           =  1
} error_t;

typedef enum {
    READ = 1,
    WRITE = 2,
    READ_WRITE = 3
} Mode;

#endif
