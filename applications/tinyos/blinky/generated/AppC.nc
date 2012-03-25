#include <hardware.h>

configuration AppC { }
implementation {
	components UserlandC;
    components new TimerMilliC() as Timer;
	components MainC, LedsC, ThreadC;
	
    UserlandC.Pal -> ThreadC.Pal;
	ThreadC.Boot -> MainC.Boot;
	ThreadC.Timer -> Timer;
	ThreadC.Leds -> LedsC;
}
