#ifndef BONGOKATHAIMAJEEZOOL
#define BONGOKATHAIMAJEEZOOL

#include <pthread.h>

#include <vector>
#include <queue>
#include <functional>

class DispatcherCallback {
public:
    void operator()();
};

class Dispatcher {
public:
    void start();
    void stop();
    void enqueue(uint32_t callback_time, DispatcherCallback* callback);

    uint32_t get_simulation_time();

private:
    void run();
    uint32_t simulation_time;
    bool running;

    typedef std::pair<uint32_t, DispatcherCallback*> Item;

    typedef std::priority_queue<Item, std::vector<Item>, std::greater<Item> > Queue;
    Queue queue;

    pthread_t thread;
    pthread_mutex_t mutex;
    pthrad_cond_t cond; 
};

extern Dispatcher dispatcher;

#endif
