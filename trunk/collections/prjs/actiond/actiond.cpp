///
///
///
#include "klwin/klwin_window.h"
#include "kl_logger.h"

#define CHECK_TIMER 1000
#define HOTKEY_ID 50111

const char *GetLogFileName()
{
	static char s_file[512];
	SYSTEMTIME time;
	GetLocalTime( &time );
	sprintf( s_file, "log/actiond_%02d-%02d-%2d-%2d-%2d.log",
			time.wMonth, time.wDay, time.wHour, time.wMinute, time.wSecond );
	return s_file;
}

class MyWindow : public klwin::Window
{
public:
	bool Init( unsigned long check_interval )
	{
		_output.open( GetLogFileName() );
		_logger.set_output( &_output );
		_logger.write( kl_common::LL_INFO, "Server start.\n" );
		::SetTimer( getHandle(), CHECK_TIMER, check_interval, NULL );
		if( ::RegisterHotKey( getHandle(), HOTKEY_ID, MOD_CONTROL | MOD_ALT, VK_LEFT ) )
		{
			_logger.write( kl_common::LL_INFO, "Register hotkey ok.\n" );
		}
		else
		{
			_logger.write( kl_common::LL_WARNING, "Register hotkey failed : %u.\n",
					::GetLastError() );
		}
	}

	void Exit()
	{
		::UnregisterHotKey( getHandle(), HOTKEY_ID );
		::KillTimer( getHandle(), CHECK_TIMER );
		_logger.write( kl_common::LL_INFO, "Server exit.\n" );
	}

	void CheckAction()
	{
		_logger.write( kl_common::LL_DEBUG, "timer elapsed.\n" );
		HWND hWnd = ::GetForegroundWindow();
		if( hWnd != NULL )
		{
			char title[512];
			int ret = ::GetWindowText( hWnd, title, sizeof( title ) );
			if( ret == 0 )
			{
				_logger.write( kl_common::LL_INFO, "Get window text failed, error code : %u.\n", 
						::GetLastError() );
			}
			else
			{
				_logger.write( kl_common::LL_INFO, "Operating on [%s].\n", title );
			}
		}
		else
		{
			_logger.write( kl_common::LL_INFO, "Get foreground window failed.\n" );
		}
	}

private:
	virtual LRESULT onMessage( UINT msg, WPARAM wParam, LPARAM lParam )
	{
		switch( msg )
		{
			case WM_TIMER:
				{	
					if( wParam == CHECK_TIMER )
					{
						CheckAction();
					}
				}
				return 0;

			case WM_HOTKEY:
				OnHotKey( wParam, lParam );
				break;

			case WM_SIZE:
				{
					if( wParam == SIZE_MINIMIZED )
					{
						this->hide();
					}
				}
				break;
		}
		return handleDefaultMessage( msg, wParam, lParam ); 
	}
	
	void OnHotKey( WPARAM wParam, LPARAM lParam )
	{
		if( wParam == HOTKEY_ID )
		{
			this->show();
			::SetForegroundWindow( this->getHandle() );
		}
	}
private:
	kl_common::logger<kl_common::file_output> _logger;
	kl_common::file_output _output;
};

int WINAPI WinMain( HINSTANCE, HINSTANCE, LPTSTR, int )
{
	MyWindow window;
	window.create( "actiond", 300, 200 );
	window.Init( 2000 );
	MSG msg;
	while( ::GetMessage( &msg, NULL, 0, 0 ) )
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	window.Exit();
	return 0;
}
