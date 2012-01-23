#include "logger.h"
#include "logger_platform.h"
#include "settings.h"
#include "dispatcher.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>

void logger_init()
{
    const char* file = settings_logger_file();
    logger_platform_init(file);
}

static char* typeString(LOG_TYPE type)
{
    switch (type) {
        case INFO: return "info";
        case WARNING: return "warning";
        case ERROR: return "error";
    }
    return "???";
}

void logger_log(LOG_TYPE type, const char* format, ...)
{
    va_list ap, ap2;
    va_start(ap, format);
    va_copy(ap2, ap);

    int innerSize = vsnprintf(NULL, 0, format, ap);
    char innerBuffer[innerSize+1];
    vsprintf(innerBuffer, format, ap2);
    va_end(ap);
    va_end(ap2);

    const char* outerFormat = "%s: %s\n";
    int outerSize = snprintf(NULL, 0, outerFormat, typeString(type), innerBuffer);
    char outerBuffer[outerSize+1];
    sprintf(outerBuffer, outerFormat, typeString(type), innerBuffer);
    
    logger_platform_out(outerBuffer);
}

int logger_syscall(const char* syscall, const char* format, ...)
{
    va_list ap, ap2;
    va_start(ap, format);
    va_copy(ap2, ap);

    static int sequence_number = 0;
    const uint32_t now = dispatcher_now();

    int innerSize = vsnprintf(NULL, 0, format, ap);
    char innerBuffer[innerSize+1];
    vsprintf(innerBuffer, format, ap2);
    va_end(ap);
    va_end(ap2);

    const char* outerFormat = "%u, %d, ->, %s, %s\n";
    int outerSize = snprintf(NULL, 0, outerFormat, now, sequence_number, syscall, innerBuffer);
    char outerBuffer[outerSize+1];
    sprintf(outerBuffer, outerFormat, now, sequence_number, syscall, innerBuffer);

    logger_platform_trace(outerBuffer); 
    return sequence_number++;
}

void logger_syscall_return(int sequence_number)
{
    const char* format = "%u, %d, <-\n";
    const uint32_t now = dispatcher_now();

    int size = snprintf(NULL, 0, format, now, sequence_number);
    char buffer[size+1];
    sprintf(buffer, format, now, sequence_number);
    logger_platform_trace(buffer);
}

char* array(uint8_t* data, size_t len)
{
    static char buffer[1024];
    size_t offset = 0;
    int size;

    size = snprintf(&buffer[offset], sizeof(buffer)-offset, "<%lu> 0x", (unsigned long int)len);
    assert (size < sizeof(buffer)-offset);
    offset += size;

    for (size_t i=0; i<len; i++) {
        size = snprintf(&buffer[offset], sizeof(buffer)-offset, "%x", data[i]);
        assert (size < sizeof(buffer)-offset);
        offset += size;
    }
    return buffer;        
}
