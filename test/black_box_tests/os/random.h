#ifndef CAEGEELAHGHEYAERAIJA
#define CAEGEELAHGHEYAERAIJA

#include "types.h"

class Random {
public:
    Random();
    int integer(int m);
    int integer(int n, int m);
    void string(char* buffer, size_t size);
    void bytes(uint8_t* buffer, size_t size);
    error_t error();
};

extern Random rnd;

#endif
