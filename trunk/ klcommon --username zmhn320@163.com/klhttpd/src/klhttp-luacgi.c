/**
  @file klhttp-internal.c
  @author Kevin Lynx
  @data 7.25.2008
  @brief for klhttpd internal use.
*/
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include "evbuffer.h"
#include <string.h>
#include "klhttp-config.h"
#include "klhttp-luacgi.h"
#include "klhttp-cgi.h"
#include "klhttp.h"
#include "klhttp-internal.h"

#ifdef CGI_LUA_SUPPORT

/**
  everytime, this module handle a cgi query, it will update these values below.
*/
static struct http_request *_request;
static struct evbuffer *_outbuf;
static struct cgi_query_string_head *_query_list;

static res_exist_cb _cb_exist;
static res_load_cb _cb_load;

/**
  the only instance lua state.
*/
static lua_State *_L;

/**
  write strings into the buffer.

  prototype : nil ()( string )
*/
static int luaC_write( lua_State *L )
{
	/* get the string argument */
	const char *content = luaL_checkstring( L, -1 );
	/* get the mime argument */
	const char *mime = luaL_checkstring( L, -2 );
	/* handle the request */
	cgi_handle_request( _outbuf, _request, mime, content, (int)strlen( content ) );

	return 0;
}

/**
  get query string.

  prototype : string ()( string )
*/
static int luaC_get_query_value( lua_State *L )
{
	/* get the name */
	const char *name = luaL_checkstring( L, -1 );
	/* get the value */
	const char *value = cgi_query_value( _query_list, name );
	/* push the return value */
	lua_pushstring( L, value );

	return 1;
}

/**
  register functions.
*/
static void luaC_register( lua_State *L )
{
	lua_pushcfunction( L, luaC_write );
	lua_setglobal( L, "cgi_write" );

	lua_pushcfunction( L, luaC_get_query_value );
	lua_setglobal( L, "cgi_query_value" );
}

/**
  lua cgi cgi callback function.
*/
static void luaC_handle_cgi_query( struct http_connection *conn, const struct http_request *request, 
					   struct cgi_query_request *query, struct cgi_query_string_head *qs, void *arg ) 
{
	int error;
	lua_State *L = (lua_State*) arg;

	/* update the status variable */
	_request = (struct http_request*) request;
	_outbuf = conn->outbuf ;
	_query_list = qs;

	/* whether the file is exist */
	error = _cb_exist( query->cgi_script );
	if( error < 0 )
	{
		http_write_info_log( "error when loading a lua script : %s : file is not exist", query->cgi_script );
		goto error ;
	}
	
	/* load it and compiles it */
	{
		struct evbuffer *buf = evbuffer_new();
		int len = _cb_load( query->cgi_script, buf );
		
		error = luaL_loadbuffer( L, EVBUFFER_DATA( buf ), len, "" );
		//evbuffer_free( buf );

		if( error != 0 )
		{
			http_write_info_log( "error when compiling a lua script : %s : %s", query->cgi_script, lua_tostring( L, -1 ) );
			http_response_error( _outbuf, HTTP_SERVUNAVAIL, CODE_STR( HTTP_SERVUNAVAIL ), "script error" );
			return ;
		}
	}

	/* execute it */
	error = lua_pcall( L, 0, 0, 0 );
	if( error != 0 )
	{
		http_write_info_log( "error when executing a lua script : %s : %s", query->cgi_script, lua_tostring( L, -1 ) );
		goto error;
	}

	return ;

error:
	http_response_error( _outbuf, HTTP_NOTFOUND, CODE_STR( HTTP_NOTFOUND ), "Canot found lua script" );
}

int luaC_init( struct http_server *httpd )
{
	_L = lua_open();
	luaL_openlibs( _L );
	luaC_register( _L );
	http_set_cgi_cb( httpd, luaC_handle_cgi_query, _L );

	return 0;
}

void luaC_release()
{
	lua_close( _L );
}

void luaC_set_script_cb( res_exist_cb cb_exist, res_load_cb cb_load )
{
	_cb_exist = cb_exist;
	_cb_load = cb_load;
}

#endif
