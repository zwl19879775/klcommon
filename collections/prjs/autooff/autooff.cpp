///
///
///
#include "klwin_window.h"
#include "win32funcs.h"
#include "utils.h"
#include <algorithm>

#define CHECK_TIMER (1001)

class MyWindow : public klwin::Window
{
public:
	bool Init()
	{
		show();
		::SetTimer( getHandle(), CHECK_TIMER, 2000, NULL );
		return true;
	}

	void Exit()
	{
		::KillTimer( getHandle(), CHECK_TIMER );
	}

	void CheckProcess()
	{
		Win32::ProcessListType pl;
		Win32::GetProcessList( &pl, true );
		if( std::find_if( pl.begin(), pl.end(), Utils::StringInSensCmp( "calc.exe" ) )
				== pl.end() )
		{
			// not exist any more, try to shut down the system
			::MessageBox( getHandle(), "Not valid any more.", "INFO", MB_OK );
			::KillTimer( getHandle(), CHECK_TIMER );
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
						CheckProcess();
					}
				}
				return 0;
		}
		return handleDefaultMessage( msg, wParam, lParam );
	}
};

int WINAPI WinMain( HINSTANCE, HINSTANCE, LPTSTR lpCmdLine, int )
{
	MyWindow window;
	window.create( "autooff", 300, 200 );
	window.Init();
	MSG msg;
	while( ::GetMessage( &msg, NULL, 0, 0 ) )
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	window.Exit();
	return 0;
}

	
