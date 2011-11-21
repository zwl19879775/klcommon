/**
  xmpputil.c
  Kevin Lynx
  6.7.2010
*/
#include <stdarg.h>
#include "xmpputil.h"

void send_chattext (xmpp_ctx_t *ctx, xmpp_conn_t *const conn, 
					xmpp_stanza_t *const stanza, const char *replytext) {
    xmpp_stanza_t *reply, *body, *text;

	reply = xmpp_stanza_new(ctx);
	xmpp_stanza_set_name(reply, "message");
	xmpp_stanza_set_type(reply, "chat");
	xmpp_stanza_set_attribute(reply, "to", xmpp_stanza_get_attribute(stanza, "from"));

	body = xmpp_stanza_new(ctx);
	xmpp_stanza_set_name(body, "body");

	text = xmpp_stanza_new(ctx);
	xmpp_stanza_set_text(text, replytext);
	xmpp_stanza_add_child(body, text);
	xmpp_stanza_add_child(reply, body);

	xmpp_send(conn, reply);
	xmpp_stanza_release(reply);
    xmpp_stanza_release(body);
    xmpp_stanza_release(text);
}

void send_formattext(xmpp_ctx_t *ctx, xmpp_conn_t *const conn,
					 xmpp_stanza_t *const stanza, const char *fmt, ...)
{
	char buf[4096];
	va_list list;
	va_start(list, fmt);
	vsprintf(buf, fmt, list);
	va_end(list);
	send_chattext(ctx, conn, stanza, buf);
}
