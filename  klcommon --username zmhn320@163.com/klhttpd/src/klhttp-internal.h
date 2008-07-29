/**
  @file klhttp-internal.h
  @author Kevin Lynx
  @data 7.25.2008
  @brief for klhttpd internal use.
*/
#ifndef ___KL_HTTP_INTERNAL_H_
#define ___KL_HTTP_INTERNAL_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/queue.h> /* the BSD queue component */

/**
  http header pair.
*/
struct http_header
{
	/** header name */
	char *name;
	/** header value */
	char *value;
	/** for the queue */
	TAILQ_ENTRY( http_header ) next;
};

/**
  http header queue head definition.
*/
TAILQ_HEAD( http_header_head, http_header );

/**
  create a new header queue.
*/
struct http_header_head *http_header_new();

/**
  free a header queue.
*/
void http_header_free( struct http_header_head *header_queue );

/**
  add one line head to the header queue.

  @param header_queue header queue to insert.
  @param line a string line contains a header pair : header-name:value.
  @return 0 if successful, otherwise return -1.
*/
int http_add_header( struct http_header_head *header_queue, char *line );

/**
  get a header value by a header name.

  @param header_queue the header queue to search in.
  @param header_name the header name.
  @return if found, return the header value, otherwise return null.
*/
const char *http_get_header_value( struct http_header_head *header_queue, const char *header_name );

/**
  http request type.
*/
enum 
{
	HTTP_GET,
	/** only retrieve the data information */
	HTTP_HEAD,
	/** unknown method, the server does not support */
	HTTP_UNKNOWN,
};

/**
  http protocol version.
*/
struct http_pro_version
{
	int major;
	int minor;
};

/**
  http request.
*/
struct http_request
{
	/** request type : GET / HEAD */
	int type;
	/** request resource */
	char *uri;
	/** http version */
	struct http_pro_version ver;
	/** http headers */
	struct http_header_head *headers;
	/** optional bodies, currently it's not implemented. */
};

/**
  parse the http request from a raw buffer from net layer.
*/
struct evbuffer;

struct http_request *http_request_parse( struct evbuffer *buf );

/**
  free the http request, you must call this function to free resources created by http_parse_request.
*/
void http_request_free( struct http_request *request );

/**
  response status code.
*/
enum
{
	HTTP_OK = 200,
	HTTP_NOTFOUND = 404,
	HTTP_BADREQUEST = 400,
	HTTP_NOTIMPLEMENT = 501,
	HTTP_SERVUNAVAIL = 503,
};

/**
  response status readable code.
*/
#define HTTP_OK_STR "OK"
#define HTTP_NOTFOUND_STR "NOT FOUND"
#define HTTP_BADREQUEST_STR "BAD REQUEST"
#define HTTP_NOTIMPLEMENT_STR "NOT IMPLEMENTED" 
#define HTTP_SERVUNAVAIL_STR "SERVER ERROR"

/** get response status readable code */
#define CODE_STR( code ) code##_STR

/**
  resource load callback.
  @return the resource file size.-1 indicates error.
*/
typedef int (*res_load_cb)( const char *uri, struct evbuffer *buf );

/** 
  resource checking callback, to make sure the resource does exist.
  @return -1 indicates not exist, 0 exist.
*/
typedef int (*res_exist_cb)( const char *uri );

/**
  handle a http request, it will write the response to the buffer.

  @param buf the output buffer, will be writen with the response.
*/
int http_handle_request( struct evbuffer *buf, const struct http_request *request, res_exist_cb cb_exist, res_load_cb cb_load );

/** 
  send error info to the client.

  @param buf the output buffer, will be writen with the response.
*/
void http_response_error( struct evbuffer *buf, int status, const char *status_str, const char *more_info );

#ifdef __cplusplus
}
#endif

#endif /* ___KL_HTTP_INTERNAL_H_ */