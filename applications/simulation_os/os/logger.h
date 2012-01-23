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
int logger_syscall(const char* syscall, const char* format, ...);
void logger_syscall_return(int sequence_number);

char* array(uint8_t* buffer, size_t len);

#endif
