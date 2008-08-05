/**
  @file klhttp-cgi.c
  @author Kevin Lynx
  @brief implement the cgi stuff
*/
#include "klhttp-config.h"
#include "klhttp-internal.h"
#include "evbuffer.h"
#include "klhttp-cgi.h"
#include <malloc.h>
#include <string.h>

#ifdef CGI_SUPPORT

#define NEW( type ) (type*) malloc( sizeof( type ) )

/**
  insert a cgi query string into the list.
*/
static void _insert_query_pair( struct cgi_query_string_head *q, const char *name, const char *value )
{
	struct cgi_query_string *qs = NEW( struct cgi_query_string );
	qs->name = (char*) malloc( strlen( name ) + 1 );
	strcpy( qs->name, name );
	qs->value = (char*) malloc( strlen( value ) + 1 );
	strcpy( qs->value, value );

	LIST_INSERT_HEAD( q, qs, next );
}

struct cgi_query_string_head *cgi_query_new()
{
	struct cgi_query_string_head *head = NEW( struct cgi_query_string_head );
	if( head == 0 )
	{
		return 0;
	}

	LIST_INIT( head );
	return head;
}

int cgi_query_parse( struct cgi_query_string_head *q, const char *query_string )
{
	const char *sep = "&";
	char *line ;
	char *pair;
	char *s = strchr( query_string, '?' );
	if( s == 0 )
	{
		s = (char*)query_string;
	}
	else
	{
		/* skip '?' */
		s++;
	}
	line = strdup( s );
	pair = strtok( line, sep );
	while( pair != 0 )
	{
		s = strchr( pair, '=' );
		if( s == 0 )
		{
			free( line );
			return -1;
		}

		*s = 0;
		_insert_query_pair( q, pair, s + 1 );
		pair = strtok( NULL, sep );
	}
	
	free( line );
	return 0;
}

const char *cgi_query_value( const struct cgi_query_string_head *q, const char *name )
{
	struct cgi_query_string *elm;
	LIST_FOREACH( elm, q, next )
	{
		if( name[0] == elm->name[0] && strcmp( name, elm->name ) == 0 )
		{
			return elm->value ;
		}
	}

	return 0;
}

void cgi_query_free( struct cgi_query_string_head *q )
{
	struct cgi_query_string *elm, *prev = 0;
	LIST_FOREACH( elm, q, next )
	{
		if( prev != 0 )
		{
			free( prev );
		}
		free( elm->name );
		free( elm->value );
		prev = elm;
	}
}

int cgi_is_query( const struct http_request *request )
{
	if( request->type == HTTP_POST )
	{
		return 1;
	}
	else
	{
		return strchr( request->uri, '?' ) != 0 ;
	}
}

struct cgi_query_request *cgi_query_request_get( struct http_request *request )
{
	struct cgi_query_request *qr = NEW( struct cgi_query_request );
	if( request->type == HTTP_GET )
	{
		/* the query string is in the uri */
		char *s = strchr( request->uri, '?' );
		qr->query_string = (char*) malloc( strlen( s + 1 ) + 1 );
		strcpy( qr->query_string, s + 1 );
		qr->cgi_script = (char*) malloc( s - request->uri + 1 );
		strncpy( qr->cgi_script, request->uri, s - request->uri );
		qr->cgi_script[ s - request->uri ] = 0;
	}
	else if( request->type == HTTP_POST )
	{
		/* the query string is in the body */
		qr->cgi_script = (char*) malloc( strlen( request->uri ) + 1 );
		strcpy( qr->cgi_script, request->uri );
		qr->query_string = (char*) malloc( EVBUFFER_LENGTH( request->body ) + 1 );
		strncpy( qr->query_string, EVBUFFER_DATA( request->body ), EVBUFFER_LENGTH( request->body ) );
		qr->query_string[EVBUFFER_LENGTH( request->body )] = 0;
	}

	return qr;
}

void cgi_query_request_free( struct cgi_query_request *qr )
{
	free( qr->cgi_script );
	free( qr->query_string );
	free( qr );
}

void cgi_handle_request( struct evbuffer *buf, struct http_request *request, const char *mime, const void *content, int len )
{
	evbuffer_add_printf( buf, "HTTP/%d.%d 200 OK\r\nContent-Type: %s\r\nContent-Length: %d\r\n"
		"Server: klhttpd/0.1.0\r\n\r\n", 
		request->ver.major, request->ver.minor, mime, len );
	evbuffer_add( buf, content, len );
}

#endif
