#ifndef ECHOOKUREISIFAEJONEE
#define ECHOOKUREISIFAEJONEE

extern volatile int debug_mark;
extern volatile int debug_line;
extern volatile char* debug_file;

#ifdef ASSERT_PRINTF
#define ASSERT(cond) do {\
    if (!(cond)) { \
        printf("ASSERTION FAILED:, file=" __FILE__ ", line=%d\n", __LINE__); \
    } } while(0)
#else
#define ASSERT(cond) do { \
    if (!(cond)) { \
        debug_file = __FILE__; \
        debug_line = __LINE__; \
        debug_mark = 0xffff; \
    } } while(0)
#endif
#endif

#include <stdio.h>
#include "ocram_cooja.h"
#define OBS(format, ...) do {\
    ENTER_PRINTF(); \
    printf("observe: " format "\n", __VA_ARGS__); \
    LEAVE_PRINTF();\
    } while(0)

