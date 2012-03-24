void foo(int);
unsigned int tc_time();
void tc_toggle_led_0();
void tc_toggle_led_1();
void tc_toggle_led_2();
typedef struct {
            void * ec_cont; unsigned int ms;
        } ec_frame_tc_sleep_t;
typedef struct {
            union {
                ec_frame_tc_sleep_t tc_sleep;
            } ec_frames;
            unsigned int now;
        } ec_frame_blink1_t;
typedef struct {
            union {
                ec_frame_tc_sleep_t tc_sleep;
            } ec_frames;
            unsigned int now;
        } ec_frame_blink2_t;
typedef struct {
            union {
                ec_frame_tc_sleep_t tc_sleep;
            } ec_frames;
            unsigned int now;
        } ec_frame_blink3_t;
ec_frame_blink1_t ec_stack_blink1;
ec_frame_blink2_t ec_stack_blink2;
ec_frame_blink3_t ec_stack_blink3;
void tc_sleep(ec_frame_tc_sleep_t *);
void ec_thread_0(void * ec_cont)
{
    if (ec_cont)
    {
        goto * ec_cont;
    }
    ec_stack_blink1.now = tc_time();
    while (1)
    {
        ec_stack_blink1.now += 250;
        ec_stack_blink1.ec_frames.tc_sleep.ms = ec_stack_blink1.now;
        ec_stack_blink1.ec_frames.tc_sleep.ec_cont = &&ec_label_blink1_1;
        tc_sleep(&ec_stack_blink1.ec_frames.tc_sleep);
        return;
    ec_label_blink1_1:
        ;
        tc_toggle_led_0();
    }
    return;
}
void ec_thread_1(void * ec_cont)
{
    if (ec_cont)
    {
        goto * ec_cont;
    }
    ec_stack_blink2.now = tc_time();
    while (1)
    {
        ec_stack_blink2.now += 500;
        ec_stack_blink2.ec_frames.tc_sleep.ms = ec_stack_blink2.now;
        ec_stack_blink2.ec_frames.tc_sleep.ec_cont = &&ec_label_blink2_1;
        tc_sleep(&ec_stack_blink2.ec_frames.tc_sleep);
        return;
    ec_label_blink2_1:
        ;
        tc_toggle_led_1();
    }
    return;
}
void ec_thread_2(void * ec_cont)
{
    if (ec_cont)
    {
        goto * ec_cont;
    }
    ec_stack_blink3.now = tc_time();
    while (1)
    {
        ec_stack_blink3.now += 1000;
        ec_stack_blink3.ec_frames.tc_sleep.ms = ec_stack_blink3.now;
        ec_stack_blink3.ec_frames.tc_sleep.ec_cont = &&ec_label_blink3_1;
        tc_sleep(&ec_stack_blink3.ec_frames.tc_sleep);
        return;
    ec_label_blink3_1:
        ;
        tc_toggle_led_2();
    }
    return;
}
