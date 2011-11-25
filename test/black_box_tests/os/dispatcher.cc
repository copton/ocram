#include <assert.h>
#include <stdlib.h>
#include <iostream>
using namespace std;

#include "dispatcher.h"

Dispatcher* Dispatcher::instance = 0;

void Dispatcher::init()
{
    char* maxtimestr = getenv("EC_MAX_TIME");
    int32_t maxtime = -1;
    if (maxtimestr == 0) {
       cerr << "warning: program runs for ever. Set EC_MAX_TIME to force program termination eventually." << endl;

    } else {
        maxtime = strtol(maxtimestr, NULL, 10);
        if (maxtime <= 0) {
            cerr << "error: EC_MAX_TIME must be a positive integer." << endl;
            exit(1);
        }
        cerr << "info: using EC_MAX_TIME " << maxtime << endl;
    }
    instance = new Dispatcher(maxtime);
}

Dispatcher::Dispatcher(int32_t maxtime) :
    maxtime(maxtime),
    simulation_time(0)
{ }

uint32_t Dispatcher::get_simulation_time()
{
    return simulation_time; 
}

void Dispatcher::enqueue(uint32_t callback_time, DispatcherCallback* callback)
{
    queue.push(std::make_pair(simulation_time + callback_time, callback));
}

bool Dispatcher::quit() const
{
    return maxtime > 0 && simulation_time > (uint32_t) maxtime;
}

void Dispatcher::run()
{
    cerr << "info: running dispatcher" << endl;
    while(!quit()) {
        assert (! queue.empty());
        const Item item = queue.top();
        queue.pop();
        if (simulation_time <= item.first) {
            simulation_time = item.first;
        } else {
            cerr << "warning: event delayed by " << simulation_time - item.first << " time units." << endl;
        }
        (*item.second)();
    }

    cerr << "info: maximum simulation time reached. Quitting." << endl;
}
