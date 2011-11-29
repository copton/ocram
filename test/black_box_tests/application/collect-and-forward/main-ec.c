#include <stdlib.h>

void ec_thread_1(void*);
void ec_thread_2(void*);
void ec_thread_3(void*);

typedef void (*thread_t)(void*);
void pal_init(int);
void pal_run();
void pal_start_thread(thread_t thread);

int main()
{
    pal_init(3);
    pal_start_thread(&ec_thread_1);
    pal_start_thread(&ec_thread_2);
    pal_start_thread(&ec_thread_3);
    pal_run();
    return 0;
}
