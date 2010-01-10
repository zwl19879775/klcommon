///
/// to notify the user it will power off soon.
///
/// kevin lynx 
/// 1.9.2010
///
#include "resource.h"
#include <windows.h>
#include <stdio.h>

#define TIME_MAX (10)
#define TIMER_ID (1002)

static long restSecs;

static void CloseDialog( HWND hWnd, UINT_PTR Ret )
{
	KillTimer( hWnd, TIMER_ID );
	EndDialog( hWnd, Ret );
}

static void UpdateNotifyText( HWND hDlg )
{
	char text[256];
	sprintf( text, "%ld 秒后将自动关机！", restSecs );
	SetDlgItemText( hDlg, IDC_TIMERINFO, text );
}

static void CALLBACK TimerProc( HWND hWnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime )
{
	restSecs --;
	UpdateNotifyText( hWnd );
	if( restSecs <= 0 )
	{
		CloseDialog( hWnd, IDOK );
	}
}

static BOOL OnInitDlg( HWND hDlg )
{
	restSecs = TIME_MAX;
	SetTimer( hDlg, TIMER_ID, 1000, TimerProc );
	UpdateNotifyText( hDlg );
	return TRUE;
}

static BOOL CALLBACK NotifyDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
	switch( msg )
	{
		case WM_INITDIALOG:
		{
			return OnInitDlg( hDlg );
		}
		break;
		
		case WM_COMMAND:
		{
			long id = LOWORD( wParam );
			if( id == IDOK )
			{
				CloseDialog( hDlg, IDOK );
			}
			else if( id == IDCANCEL )
			{
				CloseDialog( hDlg, IDCANCEL );
			}
			return TRUE;
		}
		break;

		case WM_CLOSE:
		{
			CloseDialog( hDlg, IDCANCEL );
		}
		break;
	}

	return FALSE;
}


INT_PTR notifydlg_popup( HINSTANCE inst )
{
	return DialogBox( inst, MAKEINTRESOURCE( IDD_NOTIFYDLG ), NULL, NotifyDlgProc );
}


