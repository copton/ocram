#include <assert.h>
#include <stdlib.h>
#include <iostream>
#include "random.h"

using namespace std;

Random rnd;

Random::Random() 
{
    char* seedstr= getenv("RANDOM_SEED");
    int seed;
    if (seedstr == NULL) {
		cerr << "warning: using default seed. Set RANDOM_SEED environment variable to overwrite." << endl;
        seed = 0;
    } else {
        seed = strtol(seedstr, NULL, 10);
		cerr << "info: using seed " << seed;
    }
    srand(seed);
}

// uniform distribution of [0, m]
int Random::integer(int m)
{
    assert(m >= 0);
    assert(m <= RAND_MAX);
    const int r = (RAND_MAX - m) % (m + 1);
    int value;
    do {
        value = rand();
    } while (value > RAND_MAX - r);
    return value % (m + 1);
}

// uniform distribution of [n, m]
int Random::integer(int n, int m)
{
    assert (n >= 0);
    assert (m >= n);
    return n + integer(m - n);
}

void Random::string(char* buffer, size_t size)
{
    for (int i=0; i<size-1; i++) {
        buffer[i] = 'a' + integer('z' - 'a' + 1);

    }
    buffer[size-1] = 0;
}

void Random::bytes(uint8_t* buffer, size_t size)
{
    for (int i=0; i<size; i++) {
        buffer[i] = integer(0xff);

    }
}
