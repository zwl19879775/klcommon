/*
 inject to dest process by hook.
*/

#include "bot_net.h"
#include <windows.h>
#include <stdio.h>
#include "Utils.h"
#include "UserList.h"
#include "../FeiqHelper/Setup.h"

#pragma comment(linker,"/SECTION:Shared,RWS")
#pragma data_seg ("Shared")
HHOOK g_hook = 0;
HWND g_hwnd = 0;
HINSTANCE g_dll = 0;
WNDPROC g_prevProc = NULL;
Setup g_setup;
#pragma data_seg ()

// Message communicate between inject dll and control application.
// Init inject dll.
#define MESSAGE_INIT (WM_USER + 1000)

int g_socket;

void Init() 
{
    UserList::Inst().Load();
    UserList::Inst().Dump();
    g_socket = create_socket();
}

void Release()
{
}

void BroadcastToUsers( LPCTSTR str )
{
    static bool s_waitContext = false;
    static char s_senderName[64];
    
    if( !s_waitContext )
    {
        s_waitContext = true;
        strcpy( s_senderName, str );
        return;
    }
    char buf[4096];
    s_waitContext = false;
    strcpy( buf, s_senderName );
    s_senderName[0] = '\0';
    strcat( buf, str );
    size_t size = strlen( buf );

    const UserList::UListT users = UserList::Inst().GetList();
    Log( "Start broadcast to users(%d).\n", users.size() );
    for( UserList::UListT::const_iterator it = users.begin();
        it != users.end(); ++ it )
    {
        const UserList::User &user = *it;
        Log( "Send [%s] to [%s:%d].\n", buf, user.ip, user.port );
        int ret = send_to( g_socket, user.ip, user.port, buf, size );
        Log( "Send ret : %d.\n", ret );
    }
}

BOOL APIENTRY DllMain( HMODULE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		{
			g_dll = (HINSTANCE) hModule;
		}
		break;
	case DLL_PROCESS_DETACH:
		{
		}
		break;

	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
		break;
	}
	return TRUE;
}


void WriteBinFile( const void *buf, size_t size )
{
    FILE *fp = fopen( "context.bin", "wb+" );
    if( fp == NULL ) return;
    fwrite( buf, size, 1, fp );
    fclose( fp );
}

LRESULT CALLBACK ContextWndProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    if( msg == EM_REPLACESEL )
    {
        Log( "EM_REPLACESEL message.\n" );
        //MessageBox( NULL, (LPCTSTR) lParam, "OK", MB_OK );
        BroadcastToUsers( (LPCTSTR) lParam );
    }
    
    return g_prevProc( hwnd, msg, wParam, lParam );
}


LRESULT CALLBACK GetMsgProc( int code, WPARAM wParam, LPARAM lParam )
{
    if( code >= 0 ) 
    {
        MSG *msg = (MSG*) lParam;
        // replace the window proc.
        if( (HWND) msg->hwnd == g_hwnd && g_prevProc == NULL )
        {
            Log( "Init...\n" );
            g_prevProc = (WNDPROC) GetWindowLong( g_hwnd, GWL_WNDPROC );
            SetWindowLong( g_hwnd, GWL_WNDPROC, (LONG) ContextWndProc );
            Init();
        }
    }
	return CallNextHookEx( g_hook, code, wParam, lParam );
}

#ifdef __cplusplus
extern "C"
{
#endif

bool __declspec(dllexport) HookTarget( HWND hwnd, const Setup &setup )
{
	g_hwnd = hwnd;
    g_setup = setup;
    DWORD threadId = GetWindowThreadProcessId( g_hwnd, NULL );
	g_hook = SetWindowsHookEx( WH_GETMESSAGE, GetMsgProc, g_dll, threadId );
	return g_hook != NULL;
}

bool __declspec(dllexport) UnhookTarget()
{
	return UnhookWindowsHookEx( g_hook ) == TRUE;
}

#ifdef __cplusplus
}
#endif
