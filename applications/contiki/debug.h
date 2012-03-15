#ifndef ECHOOKUREISIFAEJONEE
#define ECHOOKUREISIFAEJONEE

extern volatile int debug_mark;
extern volatile int debug_line;
extern volatile char* debug_file;

#define ASSERT(cond) do { \
    if (!(cond)) { \
        debug_file = __FILE__; \
        debug_line = __LINE__; \
        debug_mark = 0xffff; \
    } } while(0)

#endif
