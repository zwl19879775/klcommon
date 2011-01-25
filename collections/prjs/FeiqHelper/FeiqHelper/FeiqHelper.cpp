/*
*/
#include <stdio.h>
#include <windows.h>
#include "Setup.h"

#define CHATWND_CLASS "#32770"
#define CONTEXTWND_CLASS "RichEdit20A"

const char *formatTitleName( const Setup &setup )
{
    static char title[128];
    sprintf( title, "%s - Ⱥ(%s)", setup.groupName, setup.groupNumber );
    return title;
}

Setup LoadSetup()
{
    printf( "Loading setup..." );
    FILE *fp = fopen( "setup.ini", "r" );
    if( fp == NULL )
    {
        printf( "FAILED.\n" );
        exit( -1 );
    }
    Setup setup;
    fscanf( fp, "%s%s", setup.groupName, setup.groupNumber );
    fclose( fp );
    printf( "SUCCESS.\n" );
    printf( "Window name: %s\n", formatTitleName( setup ) );
    return setup;
}

HWND FindChatWindow( HWND parentWnd, const char *className, const char *wndName ) 
{
    HWND ret = NULL;
    printf( "Finding..." );
    do 
    {
        putc( '.', stdout );
        ret = FindWindowEx( parentWnd, NULL, className, wndName );
        Sleep( 1000 );
    } while( ret == NULL );
    printf( "Success\n" );
    return ret;
}

void HookDest( HWND contextWnd )
{
	HINSTANCE dll = LoadLibrary( "FeiqDll.dll" );
	if( dll == NULL )
	{
		fprintf( stderr, "failed to load FeiqDll.dll.\n" );
		exit( -1 );
	}
	typedef bool (*HookFnT)( HWND );
	typedef bool (*UnHookFnT)();
	HookFnT hook = (HookFnT)GetProcAddress( dll, "HookTarget" );
	UnHookFnT unhook = (UnHookFnT)GetProcAddress( dll, "UnhookTarget" );
	system( "pause" );
	hook( contextWnd );
	system( "pause" );
	unhook();
	FreeLibrary( dll );
}

int main()
{
    Setup setup = LoadSetup();
    HWND chatWnd = FindChatWindow( NULL, CHATWND_CLASS, formatTitleName( setup ) );
    char title[256];
    GetWindowText( chatWnd, title, sizeof(title) );
    printf( "Title : %s, ThreadId : %x\n", title, GetWindowThreadProcessId( chatWnd, NULL ) );

    HWND contextWnd = FindChatWindow( chatWnd, CONTEXTWND_CLASS, NULL );
    printf( "Handle: %x, ThreadId : %x\n", (DWORD) contextWnd, GetWindowThreadProcessId( contextWnd, NULL ) );

    HookDest( contextWnd );
    return 0;
}
