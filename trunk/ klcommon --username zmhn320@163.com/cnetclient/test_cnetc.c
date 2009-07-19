/**
 test cnetc library.
*/
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "cnetc.h"
#ifdef WIN32
#include <winsock2.h>
#endif

void error_fn( const char *fmt, ... )
{
	char buf[4096];
	va_list list;
	va_start( list, fmt );
	vsprintf( buf, fmt, list );
	va_end( list );
	fprintf( stderr, buf );
}

void notify_fn( int event, struct cnetc *cc )
{
	if( event & CC_RECV )
	{
		while( EVBUFFER_LENGTH( READ_BUF( cc ) ) )
		{
			char *str = evbuffer_readline( READ_BUF( cc ) );
			printf( str );
			free( str );
		}
	}
	if( event & CC_SEND )
	{
		printf( "send data.\n" );
	}
}

int main( int argc, char **argv )
{
#ifdef WIN32
	WSADATA wd;
	WSAStartup( MAKEWORD( 2, 0 ), &wd );
#endif
	struct cnetc *cc = cnetc_connect( argv[1], atoi( argv[2] ),
			error_fn, notify_fn  );
	if( cc == 0 )
	{
		exit( -1 );
	}
	{
		char *buf = "GET /index.html HTTP/1.1\r\n\r\n";
		evbuffer_add( WRITE_BUF( cc ), buf, strlen( buf ) );	
	}
	while( 1 )
	{
		cnetc_poll( cc, 0 );
	}
	cnetc_disconnect( cc );
#ifdef WIN32
	WSACleanup();
#endif
	return 0;
}

