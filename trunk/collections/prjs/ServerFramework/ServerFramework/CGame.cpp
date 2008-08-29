///
/// @file CGame.cpp
///
#define _CRT_SECURE_NO_WARNINGS
#include "CGame.h"
#include "App.h"

CGame *g_Game;

bool CreateGame()
{
	g_Game = new CGame();

	return g_Game != NULL;
}

CGame *GetGame()
{
	return g_Game;
}

bool DeleteGame()
{
	delete g_Game;
	g_Game = 0;

	return true;
}

CGame::CGame()
{
}

CGame::~CGame()
{
}

bool CGame::init()
{
	return true;
}

bool CGame::reload_setup()
{
	return true;
}

void CGame::release()
{
}

void CGame::process_message()
{

}

void GameMainThread( void *p )
{
#ifndef _DEBUG
	__try
	{
#endif
	kl_common::thread::param *param= (kl_common::thread::param*) p;
	kl_common::thread *thread = param->_this;

	CreateGame();
	CGame *game = GetGame();

	if( game->init() )
	{
		while( true )
		{
			/// peek the messages of the thread
			unsigned int msg = thread->handle_message();
			if( msg == kl_common::thread::WM_EXIT_THREAD )
			{
				break;
			}
		}
	}

	game->release();
	DeleteGame();

	::PostMessage( gApp.getHandle(), WM_DESTROY, 0, 0 );
#ifndef _DEBUG
	__except( Sword3::CrashFilter( GetExceptionInformation(), GetExceptionCode() ) )
	{
	}
#endif
}

void AddLogText( const char *format, ... )
{
	char msg[1024];
	va_list list;
	va_start( list, format );
	vsprintf( msg, format, list );
	va_end( list );

	gApp.addLogText( msg ); 
}
