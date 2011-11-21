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

uint32_t Random::integer(uint32_t max)
{
    return rand() % max;
}

void Random::string(unsigned char* buffer, size_t size)
{
    for (int i=0; i<size-1; i++) {
        buffer[i] = 'a' + integer('z' - 'a' + 1);

    }
    buffer[size-1] = 0;
}
