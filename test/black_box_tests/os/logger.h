#ifndef OOZOYAEQUEICHOORIPHU
#define OOZOYAEQUEICHOORIPHU

#include "types.h"

typedef enum {
    INFO,
    WARNING,
    ERROR,
} LOG_TYPE;

void logger_init();
void logger_log(LOG_TYPE type, const char* format, ...);
void logger_syscall(const char* syscall, uint32_t eta, int count, ...);

char* buffer_to_string(uint8_t* buffer, size_t len);
char* int_to_string(uint32_t value);

#endif
