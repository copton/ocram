#include <stdlib.h>

void ec_thread_1(void*);
void ec_thread_2(void*);
void ec_thread_3(void*);

void pal_init();
void pal_run();

int main()
{
    pal_init();
    ec_thread_1(NULL);
    ec_thread_2(NULL);
    ec_thread_3(NULL);
    pal_run();
    return 0;
}
