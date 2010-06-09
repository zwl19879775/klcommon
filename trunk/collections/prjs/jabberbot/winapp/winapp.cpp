/**
  winapp.cpp
  Kevin Lynx
  6.7.2010
*/
#include <windows.h>
#include "resource.h"
#include "../winplugin/win32funcs.h"
#include "log.h"
#ifdef __cplusplus
extern "C" {
#endif
#include "cmd.h"
#include "xmpputil.h"
#ifdef __cplusplus
}
#endif

#define TIMER_ID (1002)

CmdState *cs;
xmpp_ctx_t *ctx;
xmpp_conn_t *conn;
xmpp_log_t *log;

/* some basic plugin-like functions */
#define PLUGIN_NAME "self"

static int plugin_quit (const Cmd *cmd, xmpp_ctx_t *ctx,
						xmpp_conn_t *const conn, xmpp_stanza_t *const stanza) { 
    PostQuitMessage(0);
    return 1;
}

static void register_plugin (CmdState *cs) {
    cs_register(cs, PLUGIN_NAME, "quit", plugin_quit);
}

static void unregister_plugin (CmdState *cs) {
    cs_unregisterall(cs, PLUGIN_NAME);
}

void CALLBACK TimerProc( HWND hWnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime )
{
    xmpp_run_once(ctx, 1);
}

BOOL OnInitDlg( HWND hDlg )
{
	SetTimer( hDlg, TIMER_ID, 10, TimerProc );
	return TRUE;
}

void OnCloseDialog( HWND hWnd, UINT_PTR Ret )
{
	KillTimer( hWnd, TIMER_ID );
	EndDialog( hWnd, Ret );
}

BOOL CALLBACK DlgProc( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
    switch( message )
    {
    case WM_INITDIALOG:
        return OnInitDlg( hDlg );

    case WM_CLOSE:
        {
            OnCloseDialog( hDlg, wParam );	
        }
        break;
    }
    return FALSE;
}

int version_handler(xmpp_conn_t * const conn, xmpp_stanza_t * const stanza, void * const userdata)
{
	xmpp_stanza_t *reply, *query, *name, *version, *text;
	char *ns;
	xmpp_ctx_t *ctx = (xmpp_ctx_t*)userdata;
	printf("Received version request from %s\n", xmpp_stanza_get_attribute(stanza, "from"));
	
	reply = xmpp_stanza_new(ctx);
	xmpp_stanza_set_name(reply, "iq");
	xmpp_stanza_set_type(reply, "result");
	xmpp_stanza_set_id(reply, xmpp_stanza_get_id(stanza));
	xmpp_stanza_set_attribute(reply, "to", xmpp_stanza_get_attribute(stanza, "from"));
	
	query = xmpp_stanza_new(ctx);
	xmpp_stanza_set_name(query, "query");
    ns = xmpp_stanza_get_ns(xmpp_stanza_get_children(stanza));
    if (ns) {
        xmpp_stanza_set_ns(query, ns);
    }

	name = xmpp_stanza_new(ctx);
	xmpp_stanza_set_name(name, "name");
	xmpp_stanza_add_child(query, name);
	
	text = xmpp_stanza_new(ctx);
	xmpp_stanza_set_text(text, "libstrophe example bot");
	xmpp_stanza_add_child(name, text);
	
	version = xmpp_stanza_new(ctx);
	xmpp_stanza_set_name(version, "version");
	xmpp_stanza_add_child(query, version);
	
	text = xmpp_stanza_new(ctx);
	xmpp_stanza_set_text(text, "1.0");
	xmpp_stanza_add_child(version, text);
	
	xmpp_stanza_add_child(reply, query);

	xmpp_send(conn, reply);
	xmpp_stanza_release(reply);
	return 1;
}


int message_handler(xmpp_conn_t * const conn, xmpp_stanza_t * const stanza, void * const userdata)
{
	char *intext, replytext[512];
    int ret;
	xmpp_ctx_t *ctx = (xmpp_ctx_t*)userdata;
	
	if(!xmpp_stanza_get_child_by_name(stanza, "body")) return 1;
	if(!strcmp(xmpp_stanza_get_attribute(stanza, "type"), "error")) return 1;
	
	intext = xmpp_stanza_get_text(xmpp_stanza_get_child_by_name(stanza, "body"));
	
	printf("Incoming message from %s: %s\n", xmpp_stanza_get_attribute(stanza, "from"), intext);
	
    /* exec command */
    ret = cs_exe(cs, intext, ctx, conn, stanza);

    sprintf(replytext, "%s ret:%d.", intext, ret);
    send_chattext(ctx, conn, stanza, replytext);

    xmpp_free(ctx, intext);
	return 1;
}

/* define a handler for connection events */
void conn_handler(xmpp_conn_t * const conn, const xmpp_conn_event_t status, 
		  const int error, xmpp_stream_error_t * const stream_error,
		  void * const userdata)
{
    xmpp_ctx_t *ctx = (xmpp_ctx_t *)userdata;

    if (status == XMPP_CONN_CONNECT) {
	xmpp_stanza_t* pres;
	fprintf(stderr, "DEBUG: connected\n");
	xmpp_handler_add(conn,version_handler, "jabber:iq:version", "iq", NULL, ctx);
	xmpp_handler_add(conn,message_handler, NULL, "message", NULL, ctx);
	
	/* Send initial <presence/> so that we appear online to contacts */
	pres = xmpp_stanza_new(ctx);
	xmpp_stanza_set_name(pres, "presence");
	xmpp_send(conn, pres);
	xmpp_stanza_release(pres);
    }
    else {
	fprintf(stderr, "DEBUG: disconnected\n");
	xmpp_stop(ctx);
    }
}

void Init( const char *jid, const char *pass, int log_lvl )
{
    /* init library */
    xmpp_initialize();

    /* init CmdState */
    cs = cs_new();
    open_logfile();

    /* create a context */
    log = get_logger(); 
    set_loglvl(log_lvl);
    ctx = xmpp_ctx_new(NULL, log);

    /* create a connection */
    conn = xmpp_conn_new(ctx);

    /* setup authentication information */
    xmpp_conn_set_jid(conn, jid);
    xmpp_conn_set_pass(conn, pass);

    /* initiate connection */
    xmpp_connect_client(conn, NULL, 0, conn_handler, ctx);

    /* add 'self' plugin manually */
    cs_addplugin(cs, PLUGIN_NAME, 0);
    register_plugin(cs);

    /* set auto run */
    char cmd[1024], file[MAX_PATH];
    ::GetModuleFileName(NULL, file, sizeof(file));
    sprintf(cmd, "%s %s %s", file, jid, pass);
    Win32::SetAutoRun("JabberBot", cmd);
}

void Release()
{
    /* release our connection and context */
    xmpp_conn_release(conn);
    xmpp_ctx_free(ctx);

    /* final shutdown of the library */
    xmpp_shutdown();

    unregister_plugin(cs);
    cs_removeplugin(cs, PLUGIN_NAME);
    /* release the CmdState */
    cs_free(cs);
}

int WINAPI WinMain( HINSTANCE hInst, HINSTANCE, LPTSTR cmdLine, int )
{
    char jid[256], pass[256];
    int log_lvl;
    sscanf(cmdLine, "%s%s%d", jid, pass, &log_lvl);
    Init(jid, pass, log_lvl);
    //DialogBox( hInst, MAKEINTRESOURCE( IDD_MAINDLG ), NULL, DlgProc );
    HWND wnd = CreateDialog( hInst, MAKEINTRESOURCE(IDD_MAINDLG), NULL, DlgProc);
    ShowWindow(wnd, SW_HIDE);
    MSG msg;
    while(GetMessage(&msg, NULL, 0, 0))
    {
        if(!IsDialogMessage(wnd, &msg))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }
    Release();
	return 0;
}
