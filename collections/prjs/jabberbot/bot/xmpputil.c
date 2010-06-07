/**
  xmpputil.c
  Kevin Lynx
  6.7.2010
*/
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
