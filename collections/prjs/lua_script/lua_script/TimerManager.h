///
#ifndef ___TIMER_MANAGER_H_
#define ___TIMER_MANAGER_H_

#include "utils/kl_singleton.h"
#include "utils/kl_timer_queue.h"

class TimerManager : public kl_common::singleton<TimerManager>,
	public kl_common::timer_queue
{
public:
	static unsigned long NowTime();
};

#define TIME_OUT_V( t ) ( TimerManager::NowTime() + t )

#endif
