#include <stdlib.h>
#include "random.h"

void random_init()
{
    char* seedstr= getenv("RANDOM_SEED");
    int seed;
    if (seedstr == NULL) {
        seed = 0;
    } else {
        seed = strtol(seedstr, NULL, 10);
    }
    srand(seed);
}

uint32_t random_int(uint32_t max)
{
    return rand() % max;
}

void random_string(char* buffer, size_t size)
{
    for (int i=0; i<size-1; i++) {
        buffer[i] = 'a' + random_int('z' - 'a' + 1);

    }
    buffer[size-1] = 0;
}
