#ifndef CAEGEELAHGHEYAERAIJA
#define CAEGEELAHGHEYAERAIJA

#include "types.h"

void random_init();
int random_integer(int m);
int random_integer_range(int n, int m);
void random_string(char* buffer, size_t size);
void random_bytes(uint8_t* buffer, size_t size);
error_t random_error();

#endif
