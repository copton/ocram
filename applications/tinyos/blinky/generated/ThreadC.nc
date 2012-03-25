#include "Timer.h"

module ThreadC @safe()
{
  uses interface Boot;
  uses interface Timer<TMilli>;
  uses interface Leds;
  provides interface Pal;
}

implementation
{
  typedef enum {
	SYSCALL_none = 0,
	SYSCALL_sleep,
  } syscall_t;  

  typedef struct {
    syscall_t syscall;	
    union {
		ec_frame_tc_sleep_t* sleep;
    } data;
	void* ec_cont;
  } thread_t;

  thread_t thread;
  
  task void scheduler() {
  	signal Pal.cont(thread.ec_cont);
  }

  event void Boot.booted()
  {
	thread.syscall = SYSCALL_none;
	thread.ec_cont = NULL;
	post scheduler();
  }

  command void Pal.tc_sleep(ec_frame_tc_sleep_t* frame)
  {
	thread.syscall = SYSCALL_sleep;
    thread.data.sleep = frame;
    call Timer.startOneShot(call Timer.getNow() + frame->ms);
  }
 
  event void Timer.fired()
  {
    dbg("BlinkC", "Timer 0 fired @ %s.\n", sim_time_string());
    thread.ec_cont = thread.data.sleep->ec_cont;
	post scheduler();
  }

  command void Pal.tc_toggle_led_0() {
    call Leds.led0Toggle();
  } 

  command uint32_t Pal.tc_time() {
    return call Timer.getNow();
  }
}

