#include "random.h"
#include "logger.h"
#include "settings.h"

#include <assert.h>
#include <stdlib.h>

void random_init()
{
    int seed = settings_random_seed();
    logger_log(INFO, "using seed %d", seed);
    srand(seed);
}

// uniform distribution of [0, m]
int random_integer(int m)
{
    assert(m >= 0);
    assert(m <= RAND_MAX);
    const int r = (RAND_MAX - m) % (m + 1);
    int value;
    do {
        value = rand();
    } while (value > RAND_MAX - r);
    const int result = value % (m + 1);
    assert (result >= 0);
    assert (result <= m);
    return result;
}

// uniform distribution of [n, m]
int random_integer_range(int n, int m)
{
    assert (n >= 0);
    assert (m >= n);
    return n + random_integer(m - n);
}

void random_string(char* buffer, size_t size)
{
    for (size_t i=0; i<size-1; i++) {
        buffer[i] = 'a' + random_integer('z' - 'a');

    }
    buffer[size-1] = 0;
}

void random_bytes(uint8_t* buffer, size_t size)
{
    for (size_t i=0; i<size; i++) {
        buffer[i] = random_integer(0xff);
    }
}

error_t random_error()
{
    if (random_integer(1)) {
        return SUCCESS;
    } else {
        return FAIL;
    }
}
