/**
  @file klhttp.c
  @author Kevin Lynx
  @date 7.29.2008
  @brief the interface of klhttpd ( an simple embeded http server )

*/
#include "klhttp.h"
#include "evbuffer.h"
#include "klhttp-netbase.h"
#include "klhttp-internal.h"
#include <malloc.h>

#ifdef WIN32
#include <winsock2.h>
#endif

/**
  get the connection object.
*/
static struct http_connection *_get_conn( struct http_connection_head *head, int fd )
{
	struct http_connection *conn;
	LIST_FOREACH( conn, head, next )
	{
		if( fd == conn->fd )
		{
			return conn;
		}
	}

	return 0;
}

/**
  accept new client.
*/
static void _accept_conn( struct tcp_server *server, struct http_connection_head *head, int fd )
{
	int conn_fd = (int)accept( fd, 0, 0 );
	if( conn_fd < 0 )
	{
		return ;
	}

	{
		struct http_connection *conn = (struct http_connection*) malloc( sizeof( struct http_connection ) );
		conn->fd = conn_fd;
		conn->inbuf = evbuffer_new();
		conn->outbuf = evbuffer_new();

		/* add the fd into the server */
		ts_server_add( server, conn_fd );
		/* insert into the list */
		LIST_INSERT_HEAD( head, conn, next );
	}
}

/** 
  disconnect a connection.
*/
static void _disconnect_conn( struct http_connection *conn )
{
	closesocket( conn->fd );
	evbuffer_free( conn->inbuf );
	evbuffer_free( conn->outbuf );
}

/**
  handle read.
*/
static void _read_cb( int fd, void *arg )
{
	struct http_server *server = (struct http_server*)arg;
	if( fd == server->server->fd )
	{
		/* accept new client */
		_accept_conn( server->server, &server->conns, fd );
	}
	else
	{
		struct http_request *request;
		struct http_connection *conn = _get_conn( &server->conns, fd );
		/* i suppose conn will not be invalid */
		int ret = evbuffer_read( conn->inbuf, fd, 4096 );
		
		if( ret <= 0 )
		{
			/* disconnect the connection */
			ts_server_remove( server->server, fd );
			_disconnect_conn( conn );
			/* remove from the list */
			LIST_REMOVE( conn, next );
		}
		else
		{
			/* parse the request */
			request = http_request_parse( conn->inbuf );
			/* call the user-level function */
			server->r_cb( conn, request, server->arg );
			/* free the request */
			http_request_free( request );
		}
	}
}

/**
  handle write.
*/
static void _write_cb( int fd, void *arg )
{
	struct http_server *server = (struct http_server*)arg;

	struct http_connection *conn = _get_conn( &server->conns, fd );
	evbuffer_write( conn->outbuf, fd );
}

struct http_server *http_start( const char *ip, unsigned short port, int max_conn )
{
	int ret;
	struct http_server *server = (struct http_server*) malloc( sizeof( struct http_server ) );
	server->server = (struct tcp_server*) malloc( sizeof( struct tcp_server ) );
	LIST_INIT( &server->conns );
	server->r_cb = 0;
	server->arg = 0;

	ret = ts_server_startup( server->server, ip, port, max_conn, _read_cb, _write_cb, server );
	if( ret < 0 )
	{
		free( server->server );
		free( server );
		return 0;
	}

	return server;
}

void http_set_rcb( struct http_server *server, request_cb cb, void *arg )
{
	server->r_cb = cb;
	server->arg = arg;
}

void http_free( struct http_server *server )
{
	struct http_connection *conn, *prev = 0;
	ts_server_cleanup( server->server );
	LIST_FOREACH( conn, &server->conns, next )
	{
		if( prev != 0 )
		{
			free( prev );
		}
		_disconnect_conn( conn );
		prev = conn;
	}
}

int http_poll( struct http_server *server, struct timeval *timeout )
{
	return ts_server_poll( server->server, timeout );
}	