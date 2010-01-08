#include <winsock2.h>
#include <windows.h>
#include <stdio.h>
#include "tcptable.h"

#pragma comment( lib, "ws2_32.lib" )

int main()
{
	WSADATA wd;
	WSAStartup( MAKEWORD( 2, 0 ), &wd );

	Win32::TcpTable tcpTable;
	if( !tcpTable.Init() )
	{
		fprintf( stderr, "Init tcp table failed.\n" );
		exit( -1 );
	}
	Win32::TcpInfoList tcps;
	if( tcpTable.Get( tcps ) == Win32::TcpTable::FAILED )
	{
		fprintf( stderr, "Get tcp table failed.\n" );
		exit( -1 );
	}

	printf( "Process\tLocal\tRemote\tStatus\n" );
	for( size_t i = 0; i < tcps.size(); ++ i )
	{
		Win32::TcpInfo &info = tcps[i];
		char localAddr[256];
		char remoteAddr[256];
		sprintf( localAddr, "%s:%s", Win32::IpDesc( info.localAddr ), 
			Win32::PortDesc( info.localPort ) );
		sprintf( remoteAddr, "%s:%s", Win32::IpDesc( info.remoteAddr ), 
			Win32::PortDesc( info.remotePort ) );

		printf( "%s\t%s\t%s\t%s\n", info.PName, localAddr, remoteAddr, 
			Win32::StatusDesc( info.status ) );
	}

	const char *testProcess = "dnf.exe" ;
	bool ret = tcpTable.ProcessHasTcp( testProcess ) ;
	printf( "%s %s tcp connections.\n", testProcess, ret ? "has" : "hasnot" );
	tcpTable.Release();
	WSACleanup();
	return 0;
}
