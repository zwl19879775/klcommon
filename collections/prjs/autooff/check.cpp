///
/// autooff tool especially for dnf game
/// kevin lynx 
///
/// 1.8.2010 original version
///
#include <winsock2.h>
#include "win32funcs.h"
#include "tcptable.h"
#include "utils.h"
#include "check.h"

#define TIMER (1001)
#ifdef COMPILE_DIST
#define TARGET_NAME "dnf.exe"
#else
#define TARGET_NAME "calc.exe"
#endif

static Win32::TcpTable tcpTable;
static CheckConfig checkConfig;

static bool ProcessExist( const char *name )
{
	Win32::ProcessListType pl;
	Win32::GetProcessList( &pl, true );
	return std::find_if( pl.begin(), pl.end(), Utils::StringInSensCmp( name ) )
			!= pl.end() ;
}

static bool ProcessHasTcp( const char *name )
{
	return tcpTable.ProcessHasTcp( name );
}

bool CheckInit()
{
	WSADATA wd;
	if( WSAStartup( MAKEWORD( 2, 0 ), &wd ) != 0 )
	{
		return false;
	}
	return tcpTable.Init();
}

void CheckRelease()
{
	tcpTable.Release();
	WSACleanup();
}

void CALLBACK TimerProc( HWND hWnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime )
{
	if( idEvent == TIMER )
	{
		if( checkConfig.checkProcess && !ProcessExist( TARGET_NAME ) )
		{
			CheckStop( hWnd );
#ifdef COMPILE_DIST
			Win32::ShutdownSystem( checkConfig.safeShutdown );
#else
			MessageBox( hWnd, "Process does not exist", "INFO", MB_OK );
#endif
		}
		else if( checkConfig.checkNet && !ProcessHasTcp( TARGET_NAME ) )
		{
			CheckStop( hWnd );
#ifdef COMPILE_DIST
			Win32::ShutdownSystem( checkConfig.safeShutdown );
#else
			MessageBox( hWnd, "Process has not tcp conn.", "INFO", MB_OK );
#endif
		}
		else
		{
			char info[512];
			sprintf( info, "%s正在运行", TARGET_NAME );
			SetWindowText( hWnd, info );
		}
	}
}

bool CheckStart( HWND hWnd, const CheckConfig *cfg )
{
	checkConfig = *cfg;
	SetTimer( hWnd, TIMER, 2000, TimerProc );
	return true;
}

void CheckStop( HWND hWnd )
{
	KillTimer( hWnd, TIMER );
}


