///
/// autooff tool especially for dnf game
/// kevin lynx 
///
/// 1.8.2010 original version
///
#include "resource.h"
#include <windows.h>
#include <stdio.h>
#include "check.h"

#define VERSION_STR "0.2.0"
#define IDM_ABOUTBOX (0x0010)

INT_PTR uidlg_create( HINSTANCE inst );
void uidlg_destroy();

static bool checking = false;

void EnableControls( HWND hDlg, BOOL bEnable )
{
	EnableWindow( GetDlgItem( hDlg, IDC_CHECKPROCESS ), bEnable );
	EnableWindow( GetDlgItem( hDlg, IDC_CHECKNET ), bEnable );
	EnableWindow( GetDlgItem( hDlg, IDC_SAFEOFF ), bEnable );
}

#ifndef COMPILE_DIST
#define TEST_FLAG( flag ) \
	if( flag ) MessageBox( NULL, #flag, "OK", MB_OK )
#else
#define TEST_FLAG( flag ) 
#endif

void UpdateInfoText( HWND hDlg )
{
	WORD port;
	char ip[32];
	char info[256];
	if( !GetInitAddr( &port, ip ) )
	{
		sprintf( info, "获取%s网络连接信息失败！", TARGET_NAME );
	}	
	else
	{
		sprintf( info, "%s\n连接服务器信息：\n%s:%d",
				TARGET_NAME, ip, port );
	}
	SetDlgItemText( hDlg, IDC_INFOTEXT, info );
}

void OnStart( HWND hDlg )
{
	CheckConfig cfg;
	cfg.checkProcess = IsDlgButtonChecked( hDlg, IDC_CHECKPROCESS ) == BST_CHECKED;
	cfg.checkNet = IsDlgButtonChecked( hDlg, IDC_CHECKNET ) == BST_CHECKED;
	cfg.safeShutdown = IsDlgButtonChecked( hDlg, IDC_SAFEOFF ) == BST_CHECKED;

	if( !cfg.checkProcess && !cfg.checkNet )
	{
		MessageBox( hDlg, "必须选择一项检测条件！", "INFO", MB_OK | MB_ICONINFORMATION );
		return ;
	}
	else
	{
		CheckStart( hDlg, &cfg );
	}

	checking = true;
	SetDlgItemText( hDlg, IDOK, "停止" );
	EnableControls( hDlg, FALSE );

	TEST_FLAG( cfg.checkProcess );
	TEST_FLAG( cfg.checkNet );
	TEST_FLAG( cfg.safeShutdown );

	UpdateInfoText( hDlg );
}

void OnStop( HWND hDlg )
{
	checking = false;
	SetDlgItemText( hDlg, IDOK, "开始" );
	EnableControls( hDlg, TRUE );
	CheckStop( hDlg );

	SetDlgItemText( hDlg, IDC_INFOTEXT, "" );
}

BOOL OnInitDlg( HWND hDlg )
{
	CheckDlgButton( hDlg, IDC_CHECKPROCESS, BST_CHECKED );
	CheckDlgButton( hDlg, IDC_CHECKNET, BST_CHECKED );

	// append 'about' item to the system menu
	HMENU hMenu = GetSystemMenu( hDlg, FALSE );
	AppendMenu( hMenu, MF_STRING, IDM_ABOUTBOX, "关于" );	
	return TRUE;
}

void ShowAboutDlg( HWND hDlg )
{
	char info[256];
	sprintf( info, 
"\n"
"AutoOff(v%s) tool basically used for DNF game.\n"
"Author: Kevin Lynx, contact me at bbs.duowan.com.\n",
		VERSION_STR );
	MessageBox( hDlg, info, "About", MB_OK | MB_ICONINFORMATION );
}

BOOL CALLBACK DlgProc( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	switch( message )
	{
	case WM_INITDIALOG:
		{
			return OnInitDlg( hDlg );
		}
		break;

	case WM_SYSCOMMAND:
		{
			if( ( wParam & 0xfff0 ) == IDM_ABOUTBOX )
			{
				ShowAboutDlg( hDlg );
			}
			DefWindowProc( hDlg, message, wParam, lParam );
			return TRUE;
		}
		break;

	case WM_COMMAND:
		{
			long id = LOWORD( wParam );
			switch( id )
			{
			case IDOK:
				{
					if( checking )
					{
						OnStop( hDlg );
					}
					else
					{
						OnStart( hDlg );
					}
				}
				break;
			}
			return TRUE;
		}
		break;

	case WM_CLOSE:
		{
			EndDialog( hDlg, wParam );	
			CheckStop( hDlg );
		}
		break;
	}
	return FALSE;
}

INT_PTR uidlg_create( HINSTANCE inst )
{
	return DialogBox( inst, MAKEINTRESOURCE( IDD_MAINDLG ), NULL, DlgProc );
}

void uidlg_destroy()
{
}

#define UIDLG_TEST
#ifdef UIDLG_TEST
int WINAPI WinMain( HINSTANCE hInst, HINSTANCE, LPTSTR, int )
{
	CheckInit();
	uidlg_create( hInst );
	CheckRelease();
	return 0;
}
#endif

