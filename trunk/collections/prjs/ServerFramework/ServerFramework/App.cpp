/**
 * @file App.cpp
 * @author Kevin Lynx
 *
 */
#define _CRT_SECURE_NO_WARNINGS
#include "App.h"
#include "CGame.h"
#include "../res/resource.h"

/// the gloabl App object
App gApp;


/// positions to place children control.
#define MARGIN 10
#define _CLIENT_W 150

#define WINDOW_W 640
#define WINDOW_H 440

App::App() : klwin::Window()
{
}

App::~App()
{
}

bool App::init ()
{
	create( "Monster Server", WINDOW_W, WINDOW_H );
	
	/// create some gui controls
	mClientList.create( MARGIN, MARGIN, _CLIENT_W, mHeight - 2 * MARGIN, this );
	mLogBox.create( MARGIN * 2 + _CLIENT_W, MARGIN, mWidth - 3 * MARGIN - _CLIENT_W, mHeight - 2 * MARGIN, this );

	setBkColor( RGB( 128, 128, 128 ) );
	setMenu( MAKEINTRESOURCE( IDR_MAINMENU ) );
	resize( WINDOW_W, WINDOW_H );

	show();

	/// the game thread
	mGameThread.start( kl_common::thread::thread_func_type( GameMainThread ) );

	return true;
}

void App::release()
{
	/// to fix the BUG : sometimes cannot close the window.
	/// but the message function is correct, i mean the window can 
	/// receive WM_CLOSE message and WM_DESTROY message.
	static int s_rel_count = 0;
	
	if( s_rel_count >= 1 )
	{
		// the second call
		::exit( 0 );
		return ;
	}

	++ s_rel_count ;
	// Do NOT block here, if block, it will block the message callback function.
	// that will cause many fucking problems.
	mGameThread.exit( false );
}

void App::mainLoop()
{
	// when i use GetMessage and if here is true, then the window cannot update itself in time.
	// to fix this bug, just not to wait the message. 
	//while( !checkMessage( true ) )
	while( !checkMessage( false ) )
	{
		Sleep( 1 );
	}
}

void App::addClientItem( const char *ip_str, long id )
{
	char str[256];
	sprintf( str, "%d:%s", id, ip_str );

	mClientList.addString( str );
}

void App::delClientItem( const char *ip_str, long id )
{
	char str[256];
	sprintf( str, "%d:%s", id, ip_str );

	mClientList.delString( str );
}

void App::clearClientList()
{
	mClientList.clear();
}

void App::onResize( int width, int height )
{
	mClientList.move( MARGIN, MARGIN );
	mClientList.resize( _CLIENT_W, mHeight - 2 * MARGIN );

	mLogBox.move( MARGIN * 2 + _CLIENT_W, MARGIN );
	mLogBox.resize( mWidth - 3 * MARGIN - _CLIENT_W, mHeight - 2 * MARGIN );
	mLogBox.setReadOnly( true );
}

void App::addLogText( const char *format, ... )
{
	/// check whether the edit box is created
	if( mLogBox.getHandle() == NULL )
	{
		return ;
	}

	char msg[1024];
	va_list list;
	va_start( list, format );
	vsprintf( msg, format, list );
	va_end( list );

	/// replace '\n' to '\r\n' so that it can take effect.
	std::string str( msg );
	for( size_t i = 0; i < str.size(); ++ i )
	{
		if( str[i] == '\n' )
		{
			str.insert( i, "\r" );
			i += 2;
		}
	}

	mLogBox.addText( str.c_str() );
}


LRESULT App::onMessage( UINT message, WPARAM wParam, LPARAM lParam )
{
	switch( message )
	{
	case WM_SIZE:
		{
			onResize( LOWORD( lParam ), HIWORD( lParam ) );
			return 0;
		}
		break;

	case WM_COMMAND:
		{
			WORD menu_id = LOWORD( wParam );
			onMenuCommand( menu_id );
		}
		break;

	case WM_CLOSE:
		{
			/// first release the App, it will destroy everything except the Window.
			/// when the Game thread exit, it will post a WM_DESTROY message to exit the program safely.
			release();
			return 0;
		}
	case WM_DESTROY:
		{
			::PostQuitMessage( 0 );
			return 0;
		}
	}

	return klwin::Window::handleDefaultMessage( message, wParam, lParam );
}

void App::onMenuCommand( WORD id )
{
	switch( id )
	{
	case IDM_EXIT:
		onQuit();
		break;

	case IDM_RELOADSETUP:
		GetGame()->reload_setup();
		break;
	}
}

void App::onQuit()
{
	close();
}
