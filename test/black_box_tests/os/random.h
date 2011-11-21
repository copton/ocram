#ifndef CAEGEELAHGHEYAERAIJA
#define CAEGEELAHGHEYAERAIJA

#include "types.h"

class Random {
public:
    Random();
    uint32_t integer(uint32_t max);
    void string(unsigned char* buffer, size_t size);
};

extern Random rnd;

#endif
