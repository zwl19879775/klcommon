///
///
///
#include "kl_timer_queue.h"
#include <windows.h>
#include <stdio.h>

void timer_expired( void *arg )
{
	printf( "%d\n", (int)arg );
}

#pragma comment( lib, "winmm.lib" )

int main()
{
	using namespace kl_common;
	timer_queue tq;
	
	unsigned long cur_time = timeGetTime();
	const unsigned long interval = 1 * 1000;
	for( int i = 1; i <= 5; ++ i )
	{
		tq.schedule( cur_time + interval + i * 5000, timer_expired, (void*)i );
	}

	while( tq.size() > 0 )
	{
		tq.run( timeGetTime() );
	}
	
	return 0;
}
