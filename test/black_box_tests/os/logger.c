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
    va_list ap;
    va_start(ap, format);
    char buffer[1024];
    int size = vsnprintf(buffer, sizeof(buffer), format, ap);
    assert (size < sizeof(buffer));
    va_end(ap);

    logger_platform_out(typeString(type));
    logger_platform_out(": ");
    logger_platform_out(buffer); 
    logger_platform_out("\n");
}

int logger_syscall(const char* syscall, const char* format, ...)
{
    static int sequence_number = 0;

    va_list ap;
    va_start(ap, format);

    const uint32_t now = dispatcher_now();

    int innerSize = vsnprintf(NULL, 0, format, ap);
    char innerBuffer[innerSize];
    vsprintf(innerBuffer, format, ap);
    va_end(ap);

    const char* outerFormat = "%d, ->, %s, %u, %s\n";
    int outerSize = snprintf(NULL, 0, outerFormat, sequence_number, syscall, now, innerBuffer);
    char outerBuffer[outerSize];
    sprintf(outerBuffer, outerFormat, sequence_number, syscall, now, innerBuffer);

    logger_platform_trace(outerBuffer); 
    return sequence_number++;
}

void logger_syscall_return(int sequence_number)
{
    const char* format = "%d, <-, %u\n";
    const uint32_t now = dispatcher_now();

    int size = snprintf(NULL, 0, format, sequence_number, now);
    char buffer[size];
    sprintf(buffer, format, sequence_number, now);
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
