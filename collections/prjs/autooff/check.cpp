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
#include "notifydlg.h"

#define TIMER (1001)

static Win32::TcpTable tcpTable;
static CheckConfig checkConfig;
/// the initial port and ip
static WORD originalPort;
static DWORD originalAddr;

static bool ProcessExist( const char *name )
{
	Win32::ProcessListType pl;
	Win32::GetProcessList( &pl, true );
	return std::find_if( pl.begin(), pl.end(), Utils::StringInSensCmp( name ) )
			!= pl.end() ;
}

static bool ProcessTcpExist( const char *name )
{
	Win32::TcpInfoList tcps;
	size_t ret = tcpTable.Get( name, tcps );
	if( ret == 0 || ret == Win32::TcpTable::FAILED )
	{
		return false;
	}

	for( size_t i = 0; i < ret; ++ i )
	{
		if( tcps[i].remotePort == originalPort &&
			tcps[i].remoteAddr == originalAddr )
		{
			return true;
		}
	}

	return false;
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

bool GetInitAddr( WORD *port, char *ip )
{
	Win32::TcpInfoList tcps;
	size_t ret = tcpTable.Get( TARGET_NAME, tcps );
	if( ret == 0 || ret == Win32::TcpTable::FAILED )
	{
		return false;
	}
	for( size_t i = 0; i < ret; ++ i )
	{
		if( htons( tcps[i].remotePort ) > 10000 )
		{
			// specially for dnf.exe
			originalPort = tcps[i].remotePort;
			originalAddr = tcps[i].remoteAddr;
			strcpy( ip, Win32::IpDesc( originalAddr ) );
			*port = htons( originalPort );

			return true;
		}
	}	
	return false;
}

void CheckRelease()
{
	tcpTable.Release();
	WSACleanup();
}

static void DoShutdown( HWND hWnd, int reason )
{
	CheckStop( hWnd );
	INT_PTR nRet = notifydlg_popup( GetModuleHandle( NULL ) );
	if( nRet == IDCANCEL )
	{
		void OnStop( HWND hDlg );
		// i know it's ugly here.:(
		OnStop( hWnd );
		return;
	}
#ifdef COMPILE_DIST
	Win32::ShutdownSystem( checkConfig.safeShutdown );
#else
	if( reason == 1 )
	{
		MessageBox( hWnd, "Process does not exist", "INFO", MB_OK );
	}
	else if( reason == 2 )
	{
		MessageBox( hWnd, "Process has not tcp conn.", "INFO", MB_OK );
	}
#endif
}

void CALLBACK TimerProc( HWND hWnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime )
{
	if( idEvent == TIMER )
	{
		if( checkConfig.checkProcess && !ProcessExist( TARGET_NAME ) )
		{
			DoShutdown( hWnd, 1 );
		}
		else if( checkConfig.checkNet && !ProcessTcpExist( TARGET_NAME ) )
		{
			DoShutdown( hWnd, 2 );
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


