typedef struct {
	void * ec_cont; uint32_t ms;
} ec_frame_tc_sleep_t;
typedef struct {
	union {
		ec_frame_tc_sleep_t tc_sleep;
	} ec_frames;
	unsigned int now;
} ec_frame_blink1_t;

module UserlandC {
	uses interface Pal;
}


implementation {
	ec_frame_blink1_t ec_stack_blink1;

	event void Pal.cont(void* ec_cont) {
		int lbl = (int) ec_cont;
		switch (lbl) {
		case 0:
		ec_stack_blink1.now = call Pal.tc_time();
		while (1)
		{
			ec_stack_blink1.now += 250;
			ec_stack_blink1.ec_frames.tc_sleep.ms = ec_stack_blink1.now;
			ec_stack_blink1.ec_frames.tc_sleep.ec_cont = (void*) 1;
			call Pal.tc_sleep(&ec_stack_blink1.ec_frames.tc_sleep);
			return;
		case 1:
			;
			call Pal.tc_toggle_led_0();
		}
		return;
		}
	}
}
