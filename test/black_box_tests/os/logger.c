#include "logger.h"
#include "logger_platform.h"
#include "settings.h"
#include "dispatcher.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>

void logger_init()
{
    char* file = settings_logger_file();
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

    logger_platform_write(typeString(type));
    logger_platform_write(": ");
    logger_platform_write(buffer); 
    logger_platform_write("\n");
}

void logger_syscall(const char* syscall, uint32_t eta, int count, ...)
{
    va_list ap;
    va_start(ap, count);
    logger_platform_write(syscall);
    logger_platform_write(", ");
    logger_platform_write(int_to_string(dispatcher_now()));
    logger_platform_write(", ");
    logger_platform_write(int_to_string(eta));

    char* s;
    for (int i=0; i<count; i++) {
        s = va_arg(ap, char*);
        logger_platform_write(", ");
        logger_platform_write(s);
    }
    va_end(ap);
    logger_platform_write("\n");
}

char* int_to_string(uint32_t value)
{
    static char buffer[11];
    int size = snprintf(buffer, sizeof(buffer), "%d", value);
    assert (size < sizeof(buffer));
    return buffer;
}

char* buffer_to_string(uint8_t* data, size_t len)
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
