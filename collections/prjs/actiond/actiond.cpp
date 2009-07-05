///
///
///
#include "klwin/klwin_window.h"
#include "klwin/klwin_button.h"
#include "kl_logger.h"
#include <list>
#include <string>
#include <algorithm>
#include <vector>

extern std::vector<std::string> spitCmdLine();

#define CHECK_TIMER 1000
#define SHOW_HOTKEY 50111
#define CHECK_HOTKEY 50112
#define CLEAR_INTER (60*60*1000)
#define LOGPATH "actlog"

typedef std::list<std::string> TitleList;

static const char *GetLogFileName( const char *logPath )
{
	static char s_file[512];
	SYSTEMTIME time;
	GetLocalTime( &time );
	sprintf( s_file, "%s\\actiond_%02d-%02d-%2d-%2d-%2d.log",
			logPath,
			time.wMonth, time.wDay, time.wHour, time.wMinute, time.wSecond );
	return s_file;
}

static const char *GetSelfPath()
{
	static char s_path[512];
	::GetModuleFileName( NULL, s_path, sizeof( s_path ) );
	size_t len = ::strlen( s_path );
	for( size_t i = len - 1; i >= 0; -- i )
	{
		if( s_path[i] == '\\' || s_path[i] == '/' )
		{
			s_path[i] = '\0';
			break;
		}
	}
	return s_path;
}

static const char *GetLogPath( const char *logdir )
{
	static char s_path[512];
	sprintf( s_path, "%s\\%s", GetSelfPath(), logdir );
	return s_path;
}

static void SetAutoRun( const char *key, const char *cmd )
{
	HKEY regkey;
	const char *auto_key = "SOFTWARE\\Microsoft\\windows\\currentversion\\run";
	RegOpenKey( HKEY_LOCAL_MACHINE,
			auto_key, &regkey );
	RegSetValueEx( regkey, key, 0, REG_SZ, 
			(const BYTE*)cmd, strlen( cmd ) );
	RegCloseKey( regkey );	
}

class Configer
{
public:
	void ParseCmd()
	{
		std::vector<std::string> cmdline = spitCmdLine();
		if( cmdline.size() < 3 )
		{
			_checkinterval = 2000;
			_loglevel = kl_common::LL_INFO;
			return;
		}
		_checkinterval = atoi( cmdline[1].c_str() );
		_loglevel = atoi( cmdline[2].c_str() );
	}

public:
	unsigned long _checkinterval;
	int _loglevel;
};

class CachedTitle
{
public:
	CachedTitle()
	{
		_lastclear = ::timeGetTime();
	}

	bool IsInCache( const std::string &title ) const
	{
		return std::find( _cached.begin(), _cached.end(), title )
			!= _cached.end();
	}

	void AddToCache( const std::string &title )
	{
		_cached.push_back( title );
	}

	void ClearCache()
	{
		_cached.clear();
	}

	bool CheckClear()
	{
		unsigned long now = ::timeGetTime();
		if( now >= _lastclear + CLEAR_INTER )
		{
			_lastclear = now;
			ClearCache();
			return true;
		}
		return false;
	}
private:
	TitleList _cached;
	unsigned long _lastclear;
};

class MyWindow : public klwin::Window
{
public:
	MyWindow()
	{
		_checking = false;
	}
	bool Init()
	{
		InitCtls();
		const char *logPath = GetLogPath( LOGPATH );
		::CreateDirectory( logPath, NULL );	
		_configer.ParseCmd();
		_output.open( GetLogFileName( logPath ) );
		_logger.set_output( &_output );
		_logger.set_level( _configer._loglevel );
		_logger.write( kl_common::LL_INFO, "Server start.\n" );
		StartCheck();
		::RegisterHotKey( getHandle(), SHOW_HOTKEY, MOD_CONTROL | MOD_ALT, VK_LEFT ) ;
		::RegisterHotKey( getHandle(), CHECK_HOTKEY, MOD_CONTROL | MOD_ALT, VK_RIGHT ) ;

		return true;
	}

	void Exit()
	{
		::UnregisterHotKey( getHandle(), SHOW_HOTKEY );
		::UnregisterHotKey( getHandle(), CHECK_HOTKEY );
		StopCheck();
		_logger.write( kl_common::LL_INFO, "Server exit.\n" );
	}

	void StartCheck()
	{
		if( _checking )
		{
			_logger.write( kl_common::LL_INFO, "Already in check state.\n" );
		}
		else
		{
			_logger.write( kl_common::LL_INFO, "Start to check [%u].\n", 
				   _configer._checkinterval );
			::SetTimer( getHandle(), CHECK_TIMER, _configer._checkinterval, NULL );
			this->setWindowText( "ENABLED" );
			_checking = true;
		}
	}

	void StopCheck()
	{
		if( !_checking )
		{
			_logger.write( kl_common::LL_INFO, "Not in check state.\n" );
		}
		else
		{
			_logger.write( kl_common::LL_INFO, "Stop checking.\n" );
			::KillTimer( getHandle(), CHECK_TIMER );
			this->setWindowText( "DISABLED" );
			_checking = false;
		}
	}
	
	void CheckAction()
	{
		// check whether it's time to clear the title cache.
		if( _titlecache.CheckClear() )
		{
			_logger.write( kl_common::LL_DEBUG, "Cleared the cache." );
		}
		HWND hWnd = ::GetForegroundWindow();
		if( hWnd != NULL )
		{
			char title[512];
			int ret = ::GetWindowText( hWnd, title, sizeof( title ) );
			if( ret == 0 )
			{
				_logger.write( kl_common::LL_DEBUG, "Get window text failed, error code : %u.\n", 
						::GetLastError() );
			}
			else
			{
				// check whether the title is in the cache
				if( !_titlecache.IsInCache( title ) )
				{
					_logger.write( kl_common::LL_INFO, "Operating on [%s].\n", title );
					_titlecache.AddToCache( title );
				}
				else
				{
					_logger.write( kl_common::LL_DEBUG, "[%s] already in the cache.\n", title );
				}
			}
		}
		else
		{
			_logger.write( kl_common::LL_DEBUG, "Get foreground window failed.\n" );
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

			case WM_COMMAND:
				{
					if( HIWORD( wParam ) == BN_CLICKED )
					{
						OnBtnClicked( (HWND) lParam );
					}	
				}
				break;
		}
		return handleDefaultMessage( msg, wParam, lParam ); 
	}
	
	void OnHotKey( WPARAM wParam, LPARAM lParam )
	{
		if( wParam == SHOW_HOTKEY )
		{
			this->show();
			::SetForegroundWindow( this->getHandle() );
		}
		else if( wParam == CHECK_HOTKEY )
		{
			if( _checking )
			{
				StopCheck();
			}
			else
			{
				StartCheck();
			}
		}
	}

	void OnBtnClicked( HWND hBtn )
	{
		if( hBtn == _regbtn.getHandle() )
		{
			char file[512], cmd[512];
			::GetModuleFileName( NULL, file, sizeof( file ) );
			sprintf( cmd, "%s %u %d", file, 
				_configer._checkinterval, _configer._loglevel );
			SetAutoRun( "actiond", cmd );
			::MessageBox( getHandle(), "Write AUTO REG finished.", "OK",
				MB_OK );	
		}
		else if( hBtn == _logbtn.getHandle() )
		{
			const char *logPath = GetLogPath( LOGPATH );
			char cmd[512];
			sprintf( cmd, "explorer.exe %s", logPath );
			::WinExec( cmd, SW_SHOW );
		}
	}

	void InitCtls()
	{
		_regbtn.create( 30, 10, 80, 30, this );
		_regbtn.setWindowText( "WriteREG" );
		_logbtn.create( 30, 50, 80, 30, this );
		_logbtn.setWindowText( "CheckLog" );
	}
	
private:
	kl_common::logger<kl_common::file_output> _logger;
	kl_common::file_output _output;
	CachedTitle _titlecache;
	bool _checking;
	Configer _configer;
	klwin::PushButton _regbtn;
	klwin::PushButton _logbtn;
};

int WINAPI WinMain( HINSTANCE, HINSTANCE, LPTSTR lpCmdLine, int )
{
	MyWindow window;
	window.create( "actiond", 300, 200 );
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
