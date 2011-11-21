/**
  xmpputil.h
  Kevin Lynx
  6.7.2010
*/
#ifndef ___XMPP_UTIL_H_
#define ___XMPP_UTIL_H_

#include "strophe.h"

void send_chattext (xmpp_ctx_t *ctx, xmpp_conn_t *const conn, 
					xmpp_stanza_t *const stanza, const char *text);

void send_formattext(xmpp_ctx_t *ctx, xmpp_conn_t *const conn, 
					 xmpp_stanza_t *const stanza, const char *fmt, ...);
#endif
