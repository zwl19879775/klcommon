/**
  @file kl_memcached.cpp
  @author Kevin Lynx
  @brief a simple memcached client module, provides the basic ability to
  store/retrive/delete items on the memcached server.
  @date 7.17.2008
*/
#ifdef WIN32
#include <winsock2.h>
#if _MSC_VER >= 1400
#pragma warning( disable : 4996 )
#endif
#endif

#include "kl_memcached.h"

#ifdef __cplusplus
extern "C" {
#endif

/** error callback function */
static error_func_type error_func = 0;

/** notify error if error_func has been set */
#define NOTIFY_ERROR( err_code, desc ) \
	if( error_func != 0 ) \
		error_func( err_code, desc )

/** retrieve the last error on windows. */
#ifdef WIN32
#define LAST_ERROR WSAGetLastError()
#else
#define LAST_ERROR (-1)
#endif

/** format a string */
static const char *FORMAT_STR( const char *fmt, ... )
{
	static char buf[1024];
	va_list list;
	va_start( list, fmt );
	vsprintf( buf, fmt, list );
	va_end( list ); 
	return buf;
}

/** recv response message from memcached server */
static int get_reply( int fd, unsigned int *value )
{
	char msg[256];
	/* get the reply message */
	int ret = recv( fd, msg, sizeof( msg ) - 1, 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "get reply failed" );
		return MEM_SYSTEM_ERROR;
	}

	/* parse the reply message */
	msg[ret] = 0;
#define CHECK_REPLY( s, c ) if( strcmp( msg, s ) == 0 ) return c
	CHECK_REPLY( "ERROR\r\n", MEM_ERROR );
	CHECK_REPLY( "CLIENT_ERROR\r\n", MEM_CLIENT_ERROR );
	CHECK_REPLY( "SERVER_ERROR\r\n", MEM_SERVER_ERROR );
	CHECK_REPLY( "STORED\r\n", MEM_STORED );
	CHECK_REPLY( "NOT_STORED\r\n", MEM_NOT_STORED );
	CHECK_REPLY( "EXISTS\r\n", MEM_EXISTS );
	CHECK_REPLY( "NOT_FOUND\r\n", MEM_NOT_FOUND );
	CHECK_REPLY( "DELETED\r\n", MEM_DELETED );
	CHECK_REPLY( "OK\r\n", MEM_OK );

	/* for incr/decr command */
	if( value != 0 )
	{
		*value = atoi( msg );
	}
	return MEM_OK;
}

/** parse the 'get' command reply : VALUE <key> <flags> <bytes> */
static int parse_get_reply( struct item_data *item, const char *header, size_t *bytes )
{
#define REPLY_ERROR( p ) \
	if( p == 0 ) \
	{ \
		NOTIFY_ERROR( -1, "get reply error" ); \
		return -1; \
	}

	char *p = strstr( header, " " );
	REPLY_ERROR( p );
	if( strcmp( p + 1, item->_key ) == 0 )
	{
		NOTIFY_ERROR( -1, FORMAT_STR( "key [%s] dose not match to key [%s]", item->_key, p + 1 ) );
		return -1;
	}

	p = strstr( p + 1, " " );
	REPLY_ERROR( p );

	{
		/* get the flag */
		char flag[32];
		char *pend ;
		strcpy( flag, p + 1 );
		pend = strstr( flag, " " );
		REPLY_ERROR( pend );
		*pend = 0;
		p = pend + 1;

		item->_flag = atol( flag );
	}

	{
		/* get the bytes */
		*bytes = atoi( p );		
	}

	return 0;
}

void mem_set_error_handler( error_func_type func )
{
	error_func = func;
}

int mem_connect( struct mem_server *server, const char *addr, unsigned short port )
{
	server->_fd = (int) socket( AF_INET, SOCK_STREAM, 0 );
	memset( &server->_addr, 0, sizeof( server->_addr ) );
	server->_addr.sin_family = AF_INET;
	server->_addr.sin_port = htons( port );
	server->_addr.sin_addr.s_addr = inet_addr( addr ) ;

	if( server->_addr.sin_addr.s_addr == INADDR_NONE )
	{
		NOTIFY_ERROR( LAST_ERROR, "invalid address" );
		return -1;
	}

	{
		int ret = connect( server->_fd, (struct sockaddr*)&server->_addr, sizeof( server->_addr ) );
		if( ret == -1 )
		{
			NOTIFY_ERROR( LAST_ERROR, "connect to the server failed" );
			return -1;
		}
	}

	return 0;
}

void mem_disconnect( const struct mem_server *server )
{
	closesocket( server->_fd );
}

int mem_store( const struct mem_server *server, const struct store_item *item, int need_response, const char *store_type )
{
	/* send command */
	int ret;
	char cmd[256];
	int fd = server->_fd;

	struct evbuffer *data = item->_item._buffer ;
	size_t data_len = EVBUFFER_LENGTH( data );
	
	/* <command> <key> <flag> <exp time> <bytes> */
	if( need_response == 0 )
	{
		sprintf( cmd, "%s %s %d %d %d noreply\r\n", store_type, item->_item._key, item->_item._flag, item->_exp_time, data_len );
	}
	else
	{
		sprintf( cmd, "%s %s %d %d %d\r\n", store_type, item->_item._key, item->_item._flag, item->_exp_time, data_len );
	}

	ret = send( fd, cmd, (int)strlen( cmd ), 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "send command failed" );
		return MEM_SYSTEM_ERROR;
	}
	
	/* send data */
	ret = evbuffer_write( data, fd );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "send data failed" );
		return MEM_SYSTEM_ERROR;
	}
	ret = send( fd, "\r\n", 2, 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "send data terminated failed" );
		return MEM_SYSTEM_ERROR;
	}
	
	/* get response */
	if( need_response != 0 )
	{
		ret = get_reply( fd, 0 );
	}
	else
	{
		ret = MEM_NO_RESPONSE;
	}

	return ret;
}

int mem_gets( const struct mem_server *server, struct item_data *items[], size_t size )
{
	char cmd[1024];
	size_t i;
	int ret;
	int fd = server->_fd;

	strcpy( cmd, "get" );
	for( i = 0; i < size; ++ i )
	{
		strcat( cmd, " " );
		strcat( cmd, items[i]->_key );
	}
	strcat( cmd, "\r\n" );

	/* send command */
	ret = send( fd, cmd, (int)strlen( cmd ), 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "send command failed" );
		return MEM_SYSTEM_ERROR;
	}

	/* get data */
	{
		struct evbuffer *chunk = evbuffer_new();
		size_t bytes = 0;
		ret = evbuffer_read( chunk, fd, 4096 );

		for( i = 0; i < size; ++ i )
		{
			/* get header */
			char *header = evbuffer_readline( chunk );
			
			/* check END */
			if( strcmp( header, "END" ) == 0 )
			{
				free( header );
				break;
			}

			if( parse_get_reply( items[i], header, &bytes ) == -1 )
			{
				free( header );
				evbuffer_free( chunk );
				return MEM_SYSTEM_ERROR;
			}
			free( header );

			/* get data */
			{
				char *data = (char*)malloc( bytes );
				evbuffer_remove( chunk, data, bytes );
				evbuffer_add( items[i]->_buffer, data, bytes );
				free( data );

				/* skip the '\r\n' */
				evbuffer_readline( chunk );
			}
		}

		evbuffer_free( chunk );
	}

	return (int)i;
}

int mem_get( const struct mem_server *server, struct item_data *item )
{
	return mem_gets( server, &item, 1 );
}

int mem_delete( const struct mem_server *server, const char key[256], int need_response )
{
	char cmd[512];
	int fd = server->_fd;
	int ret;

	/* delete key */
	if( need_response == 0 )
	{
		sprintf( cmd, "delete %s noreply\r\n", key );
	}
	else
	{
		sprintf( cmd, "delete %s\r\n", key );
	}

	ret = send( fd, cmd, (int)strlen( cmd ), 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "send command failed" );
		return MEM_SYSTEM_ERROR;
	}

	if( need_response == 0 )
	{
		ret = MEM_NO_RESPONSE;
	}
	else
	{
		ret = get_reply( fd, 0 );
	}

	return ret;
}

int mem_inc_dec( const struct mem_server *server, const char key[256], unsigned int value, unsigned int *new_value, const char *cmd_type )
{
	/* incr/decr/ key value */
	char cmd[512];
	int fd = server->_fd;
	int ret;

	if( new_value != 0 )
	{
		/* need reply */
		sprintf( cmd, "%s %s %u\r\n", cmd_type, key, value );
	}
	else
	{
		sprintf( cmd, "%s %s %u noreply\r\n", cmd_type, key, value );
	}

	ret = send( fd, cmd, (int)strlen( cmd ), 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "send command failed" );
		return MEM_SYSTEM_ERROR;
	}

	/* get reply - new value */
	if( new_value != 0 )
	{
		ret = get_reply( fd, new_value );
	}
	else
	{
		ret = MEM_NO_RESPONSE;
	}

	return ret;
}

int mem_stats( const struct mem_server *server, char *stat_desc, size_t size )
{
	char cmd[] = "stats\r\n" ;
	int ret = send( server->_fd, cmd, (int)strlen( cmd ), 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "send command failed" );
		return MEM_SYSTEM_ERROR;
	}

	ret = recv( server->_fd, stat_desc, (int)size - 1, 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "recv reply failed" );
		return MEM_SYSTEM_ERROR;
	}

	stat_desc[ret] = 0;
	return MEM_OK;
}

int mem_flush_all( const struct mem_server *server )
{
	char cmd[] = "flush_all\r\n";
	int ret = send( server->_fd, cmd, (int)strlen( cmd ), 0 );
	if( ret == -1 )
	{
		NOTIFY_ERROR( LAST_ERROR, "send command failed" );
		return MEM_SYSTEM_ERROR;
	}

	ret = get_reply( server->_fd, 0 );
	return ret;
}

#ifdef __cplusplus
}
#endif
