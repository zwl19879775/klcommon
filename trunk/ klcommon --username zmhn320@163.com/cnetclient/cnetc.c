/**
 @file cnetc.c
 @author Kevin Lynx
 @brief A simple c version wrapper for select,only for client application.
*/
#include "cnetc.h"
#ifdef WIN32
#include <winsock2.h>
#endif
#include <stdlib.h>

#ifdef __cplusplus
extern "C"
{
#endif

struct cnetc_impl
{
	struct cnetc base;
	struct fd_set fd_read;
	struct fd_set fd_write;
};

#define CAST( type, var ) (type)( var )
#define CAST_CNETC( var ) CAST( struct cnetc*, var ) 
#define CAST_CNETC_IMPL( var ) CAST( struct cnetc_impl*, var )
#define BASE( impl_ptr ) (impl_ptr->base)

static struct sockaddr_in *to_addr( const char *ip, unsigned short port )
{
	static struct sockaddr_in addr;
	memset( &addr, 0, sizeof( addr ) );
	addr.sin_family = AF_INET;
	addr.sin_port = htons( port );
	addr.sin_addr.s_addr = inet_addr( ip );
	return &addr;
}

static void set_fd( struct cnetc_impl *cc_impl )
{
	FD_ZERO( &cc_impl->fd_read );
	FD_ZERO( &cc_impl->fd_write );
	FD_SET( BASE( cc_impl ).fd, &cc_impl->fd_read );
	FD_SET( BASE( cc_impl ).fd, &cc_impl->fd_write );
}

struct cnetc *cnetc_connect( const char *sip, unsigned short sport, 
		void (*err_log)( const char *fmt, ... ),
	   	void (*notify_fn)( int, struct cnetc *, int bytes ) )
{
	struct cnetc_impl *cc = (struct cnetc_impl*) malloc( sizeof( struct cnetc_impl ) );
	BASE( cc ).connect_flag = 0;
	BASE( cc ).err_log = err_log;
	BASE( cc ).notify_fn = notify_fn;
	BASE( cc ).read_buf = evbuffer_new();
	BASE( cc ).write_buf = evbuffer_new();
	BASE( cc ).fd = socket( AF_INET, SOCK_STREAM, 0 );
	if( cnetc_reconnect( CAST_CNETC( cc ), sip, sport ) != 0 )
	{
		cnetc_disconnect( CAST_CNETC( cc ) );
		return 0;
	}
	return CAST_CNETC( cc );
}

int cnetc_reconnect( struct cnetc *cc, const char *sip, unsigned short sport )
{
	struct cnetc_impl *cc_impl = CAST_CNETC_IMPL( cc );
	struct sockaddr_in *addr = to_addr( sip, sport );
	if( BASE( cc_impl ).connect_flag )
	{
		closesocket( BASE( cc_impl ).fd );
		BASE( cc_impl ).connect_flag = 0;
	}
	if( connect( BASE( cc_impl ).fd, (struct sockaddr*)addr, 
		sizeof( *addr ) ) == -1 )
	{
		cc->err_log( "connect to %s-%u failed.\n", sip, sport );
		return -1;
	}	
	BASE( cc_impl ).connect_flag = 1;
	BASE( cc_impl ).sip = addr->sin_addr.s_addr;
	BASE( cc_impl ).sport = sport;
	return 0;
}

int cnetc_poll( struct cnetc *cc, unsigned long sec )
{
	struct cnetc_impl *cc_impl = CAST_CNETC_IMPL( cc );
	struct timeval timeout;
	int ev = 0;
	int bytes = 0;
	set_fd( cc_impl );
	memset( &timeout, 0, sizeof( timeout ) );
	timeout.tv_sec = sec;
	if( select( 0, &cc_impl->fd_read, &cc_impl->fd_write, 0, &timeout ) <= 0 )
	{
		return 0;
	}
	if( FD_ISSET( cc->fd, &cc_impl->fd_read ) )
	{
		ev |= CC_RECV;
		bytes = evbuffer_read( cc->read_buf, cc->fd, 4096 );
	}
	if( EVBUFFER_LENGTH( cc->write_buf ) && FD_ISSET( cc->fd, &cc_impl->fd_write ) )
	{
		ev |= CC_SEND;
		bytes = evbuffer_write( cc->write_buf, cc->fd );
	}
	if( ev != CC_NONE )
	{
		cc->notify_fn( ev, cc, bytes );
	}
	return 0;
}

void cnetc_disconnect( struct cnetc *cc )
{
	struct cnetc_impl *cc_impl = CAST_CNETC_IMPL( cc );
	closesocket( BASE( cc_impl ).fd );
	evbuffer_free( BASE( cc_impl ).read_buf );
	evbuffer_free( BASE( cc_impl ).write_buf );
	free( cc_impl );
}

#ifdef __cplusplus
}
#endif

