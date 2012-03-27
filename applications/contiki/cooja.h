#ifndef ECHOOKUREISIFAEJONEE
#define ECHOOKUREISIFAEJONEE

extern volatile int debug_mark;
extern volatile int debug_line;
extern volatile char* debug_file;

#ifdef ASSERT_PRINTF
#include <stdio.h>
#endif

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

#ifdef OBSERVE_BEHAVIOR
#include <stdio.h>
#define OBS(format, ...) printf("observable behavior" format "\n", __VA_ARGS__)
#else
#define OBS(...)
#endif
