#ifndef BONGOKATHAIMAJEEZOOL
#define BONGOKATHAIMAJEEZOOL

#include <vector>
#include <queue>
#include <functional>

#include "types.h"

class DispatcherCallback {
public:
    virtual void operator()() =0;
};

class Dispatcher {
public:
    static Dispatcher* instance;
    static void init();

    void run();
    void enqueue(uint32_t callback_time, DispatcherCallback* callback);
    uint32_t get_simulation_time();

private:
    Dispatcher();

    uint32_t simulation_time;
    bool running;

    typedef std::pair<uint32_t, DispatcherCallback*> Item;
    typedef std::priority_queue<Item, std::vector<Item>, std::greater<Item> > Queue;
    Queue queue;
};

#endif
