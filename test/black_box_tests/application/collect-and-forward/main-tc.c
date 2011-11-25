#include "os/tc.h"

void task_send();
void task_receive();
void task_collect();

int main()
{
    tc_init();

	tc_run_thread(&task_send);
    tc_run_thread(&task_receive);
    tc_run_thread(&task_collect);

    tc_run();

    return 0;
}
