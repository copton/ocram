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
    class Quit { };

    static Dispatcher* instance;
    static void init();

    void run();
    bool quit() const;
    void enqueue(uint32_t callback_time, DispatcherCallback* callback);
    uint32_t get_simulation_time();

private:
    Dispatcher(int32_t maxtime);

    const int32_t maxtime;
    uint32_t simulation_time;

    typedef std::pair<uint32_t, DispatcherCallback*> Item;
    typedef std::priority_queue<Item, std::vector<Item>, std::greater<Item> > Queue;
    Queue queue;
};

#endif
