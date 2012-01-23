#include "logger_platform.h"

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>

int fd;

void logger_platform_init(const char* file)
{
    fd = open(file, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
    if (fd == -1) {
        fprintf(stderr, "failed to open '%s': %s'\n", file, strerror(errno));
        exit(1);
    }
}

void logger_platform_trace(const char* text)
{
    write(fd, text, strlen(text));
}

void logger_platform_out(const char* text)
{
    write(1, text, strlen(text));
}
