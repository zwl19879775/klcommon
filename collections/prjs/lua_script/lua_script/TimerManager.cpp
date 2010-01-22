///
#include "TimerManager.h"
#include <windows.h>

unsigned long TimerManager::NowTime()
{
	return timeGetTime();
}
