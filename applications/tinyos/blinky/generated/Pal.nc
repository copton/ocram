interface Pal {
    command void tc_toggle_led_0();
	command void tc_sleep(ec_frame_tc_sleep_t* frame);
    command uint32_t tc_time();
	event void cont(void* ec_cont);
}
