#ifndef ISEIVAHTHAEVAEGEIGHE
#define ISEIVAHTHAEVAEGEIGHE

#ifdef OCRAM_MODE
#define TC_RUN_THREAD __attribute__((tc_run_thread))
#define TC_BLOCKING __attribute__((tc_blocking))
#else
#define TC_RUN_THREAD
#define TC_BLOCKING
#endif

typedef void (*ThreadExecutionFunction)(void*);

#endif
