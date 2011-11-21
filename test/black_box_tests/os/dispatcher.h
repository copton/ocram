#ifndef BONGOKATHAIMAJEEZOOL
#define BONGOKATHAIMAJEEZOOL

#include <vector>
#include <queue>
#include <functional>

#include "types.h"

class DispatcherCallback {
public:
    void operator()();
};

class Dispatcher {
public:
    Dispatcher();
    void run();
    void enqueue(uint32_t callback_time, DispatcherCallback* callback);

    uint32_t get_simulation_time();

private:
    uint32_t simulation_time;
    bool running;

    typedef std::pair<uint32_t, DispatcherCallback*> Item;

    typedef std::priority_queue<Item, std::vector<Item>, std::greater<Item> > Queue;
    Queue queue;
};

extern Dispatcher dispatcher;

#endif
