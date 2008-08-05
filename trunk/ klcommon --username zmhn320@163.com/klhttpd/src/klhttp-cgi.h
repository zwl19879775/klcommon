/**
  @file klhttp-cgi.h
  @author Kevin Lynx
  @brief implement the cgi stuff
*/
#ifndef ___KLHTTP_CGI_H_
#define ___KLHTTP_CGI_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/queue.h> /* the BSD queue component */

/**
  cgi query strings.
*/
struct cgi_query_string
{
	char *name;
	char *value;
	LIST_ENTRY( cgi_query_string ) next;
};

LIST_HEAD( cgi_query_string_head, cgi_query_string );

/**
  malloc memory for cgi_query_string list.
*/
struct cgi_query_string_head *cgi_query_new();

/**
  parse the query string from url or http request body, this function will create a cgi_query_string list.

  The query string must be decoded already.
*/
int cgi_query_parse( struct cgi_query_string_head *q, const char *query_string );

/**
  free the cgi_query_string list.
*/
void cgi_query_free( struct cgi_query_string_head *q );

/**
  get a cgi query value by its name.
*/
const char *cgi_query_value( const struct cgi_query_string_head *q, const char *name );

struct http_request;
struct evbuffer;

/**
  check the http request whether is a cgi query.
  
  The query string must be decoded already.

  @return if the uri is a query string ,return non-zero, otherwise return 0.
*/
int cgi_is_query( const struct http_request *request ) ;

/**
  cgi query request.
*/
struct cgi_query_request
{
	/** cgi query string like : name=kevin lynx&age=22 */
	char *query_string;
	/** cgi query script which will handle the query, it's an execuable script */
	char *cgi_script;
};

/**
  get cgi_query_request from http request.

  You must call free the return value.
*/
struct cgi_query_request *cgi_query_request_get( struct http_request *request );

/**
  free the cgi_query_request.
*/
void cgi_query_request_free( struct cgi_query_request *qr );

/**
  a helper function to write cgi response.

  @param buf output buffer.
  @param mime mime type like : text/html
  @len content length
*/
void cgi_handle_request( struct evbuffer *buf, struct http_request *request, const char *mime, const void *content, int len );

#ifdef __cplusplus
}
#endif

#endif /* ___KLHTTP_CGI_H_ */