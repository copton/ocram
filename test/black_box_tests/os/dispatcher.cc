#include <assert.h>
#include <iostream>

#include "dispatcher.h"

Dispatcher dispatcher;

Dispatcher::Dispatcher() :
    simulation_time(0)
{
    pthread_mutex_init(&mutex, NULL);
    pthread_cond_init(&cond, NULL);
}

void Dispatcher::start()
{
    assert (running != true);
    running = true;
    pthread_create(&thread, NULL, run_thread, this);
}

void Dispatcher::stop()
{
    pthread_mutex_lock(&mutex);
    assert (running == true);
    running = false;
    pthread_mutex_unlock(&mutex);
    phread_join(&thread, NULL);
}

static void* run_thread(void* args)
{
    Dispatcher* dispatcher = (Dispatcher*) args;
    dispatcher->run();
    return NULL;
}

uint32_t get_simulation_time()
{
    pthread_mutex_lock(&mutex);
    const uint32_t res = simulation_time; 
    pthread_mutex_unlock(&mutex);
    return res;
}

void Dispatcher::enqueue(uint32_t callback_time, DispatcherCallback* callback)
{
    pthread_mutex_lock(&mutex);
    queue.push(std::make_pair(simulation_time + callback_time, callback));
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);
}

void Dispatcher::run()
{
    while(1) {
        pthread_mutex_lock(&mutex);
        if (running == false) {
            break;
        }
        while (queue.empty()) {
            pthread_cond_wait(&cond, &mutex);
        }
        const Item item = queue.top();
        queue.pop();
        if (simulation_time <= item.first) {
            simulation_time = item.first;
        } else {
            cerr << "warning: event delayed by " << simulation_time - item.first << " time units." << endl;
        }
        pthread_mutex_unlock(&mutex);

        (*item.second)();
    }
}
