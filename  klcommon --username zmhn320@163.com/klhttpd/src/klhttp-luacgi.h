/**
  @file klhttp-luacgi.h
  @author Kevin Lynx
  @date 8.8.2008
  @brief to use lua to handle cgi query.
*/
#ifndef ___KL_HTTP_LUACGI_H_
#define ___KL_HTTP_LUACGI_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "klhttp-internal.h"

/**
  init lua-cgi sub system.

  This function will register some functions in lua script.
*/
int luaC_init( struct http_server *httpd );

/**
  shutdown the lua-cgi sub system.
*/
void luaC_release();

/**
  set resource handle callback.They will be used to load the lua script.
*/
void luaC_set_script_cb( res_exist_cb cb_exist, res_load_cb cb_load );

#ifdef __cplusplus
}
#endif

#endif /* ___KL_HTTP_LUACGI_H_ */