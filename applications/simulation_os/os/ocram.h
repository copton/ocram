#ifndef ISEIVAHTHAEVAEGEIGHE
#define ISEIVAHTHAEVAEGEIGHE

#ifdef OCRAM_MODE
#define TC_RUN_THREAD __attribute__((tc_thread))
#define TC_BLOCKING __attribute__((tc_block))
#else
#define TC_RUN_THREAD
#define TC_BLOCKING
#endif

#endif
