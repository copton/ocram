#include "dispatcher.h"
#include "settings.h"
#include "logger.h"

#include <assert.h>

int32_t max_time;
int32_t simulation_time;

typedef struct {
    uint32_t callback_time;
    DispatcherCallback callback;
    void* context;
} Unit;

#define QUEUE_MAX 10
Unit queue[QUEUE_MAX];
int begin;
int end;

static void queue_init()
{
    begin = 0;
    end = 0;
}

static bool queue_full()
{
    return (end + 1) % QUEUE_MAX == begin;
}

static bool queue_empty()
{
    return begin == end;
}

static void queue_push(Unit unit)
{
    assert (! queue_full());
    queue[end] = unit;
    end = (end + 1) % QUEUE_MAX;
}

static Unit queue_pop()
{
    assert (! queue_empty());
    int min = begin;
    for (int i=(begin+1)%QUEUE_MAX; i!=end; i = (i+1)%QUEUE_MAX) {
        if (queue[i].callback_time < queue[min].callback_time) {
            min = i;
        }
    }
    Unit result = queue[min];
    if (min != begin) {
        queue[min] = queue[begin];
    }
    begin = (begin + 1) % QUEUE_MAX;
    return result;
}

void dispatcher_init()
{
    queue_init();
    max_time = settings_dispatcher_max_time();
    simulation_time = 0;
}

void dispatcher_enqueue(int32_t callback_time, DispatcherCallback callback, void* context)
{
    Unit unit = {callback_time, callback, context};
    queue_push(unit);
}

bool dispatcher_quit()
{
    return max_time > 0 && simulation_time > max_time;
}

uint32_t dispatcher_now()
{
    return simulation_time;
}

void dispatcher_run()
{
    logger_log(INFO, "running dispatcher");
    while(!dispatcher_quit()) {
        Unit unit = queue_pop();
        assert (simulation_time <= unit.callback_time);
        simulation_time = unit.callback_time;
        unit.callback(unit.context);
    }
    logger_log(INFO, "maximum simulation time reached. Quitting.");
}
