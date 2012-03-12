#ifndef FOHDIECOVAZEEPAUNEEJ
#define FOHDIECOVAZEEPAUNEEJ

void tl_init();
int tl_create_thread(void (*fcn)(), uint16_t * stack_ptr);
void tl_sleep(int ms);
void tl_wakeup(uint8_t id);

#endif
