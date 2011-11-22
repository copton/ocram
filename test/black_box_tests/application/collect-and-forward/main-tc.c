#include "os/tc.h"

void task_send();
void task_receive();
void task_collect();

int main()
{
	const int t1 = tc_run_thread(&task_send);
    const int t2 = tc_run_thread(&task_receive);
    const int t3 = tc_run_thread(&task_collect);

    tc_join_thread(t1);
    tc_join_thread(t2);
    tc_join_thread(t3);

    return 0;
}
