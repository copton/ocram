#include <assert.h>
#include <iostream>
using namespace std;

#include "dispatcher.h"

Dispatcher* Dispatcher::instance = 0;

void Dispatcher::init()
{
    instance = new Dispatcher();
}

Dispatcher::Dispatcher() :
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

void Dispatcher::run()
{
    cerr << "info: running dispatcher" << endl;
    while(!queue.empty()) {
        const Item item = queue.top();
        queue.pop();
        if (simulation_time <= item.first) {
            simulation_time = item.first;
        } else {
            cerr << "warning: event delayed by " << simulation_time - item.first << " time units." << endl;
        }
        (*item.second)();
    }
    cerr << "info: dispatcher quits" << endl;
}
