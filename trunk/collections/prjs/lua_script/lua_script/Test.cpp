
#include "ScriptSys.h"
#include "TimerManager.h"
#include <stdio.h>


int main()
{
	ScriptSys *scriptSys = new ScriptSys();
	TimerManager *timer = new TimerManager();
	scriptSys->Init();

	scriptSys->RunScript( "scripts\\1.lua" );
	scriptSys->RunScript( "scripts\\2.lua" );
	scriptSys->RunStr( "print('RunStr')" );

	// yield script test.
	scriptSys->RunScript( "scripts\\3.lua" );
	while( timer->size() )
	{
		timer->run( TimerManager::NowTime() );		
	}

	scriptSys->Release();
	delete timer;
	delete ScriptSys::getSingletonPtr();
	return 0;
}
