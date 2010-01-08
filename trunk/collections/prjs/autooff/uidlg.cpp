///
/// autooff tool especially for dnf game
/// kevin lynx 
///
/// 1.8.2010 original version
///
#include "resource.h"
#include <windows.h>
#include "check.h"

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
}

void OnStop( HWND hDlg )
{
	checking = false;
	SetDlgItemText( hDlg, IDOK, "开始" );
	EnableControls( hDlg, TRUE );
	CheckStop( hDlg );
}

BOOL CALLBACK DlgProc( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	switch( message )
	{
	case WM_INITDIALOG:
		{
			CheckDlgButton( hDlg, IDC_CHECKPROCESS, BST_CHECKED );
			CheckDlgButton( hDlg, IDC_CHECKNET, BST_CHECKED );
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

