/**
  windows plugin 
  Kevin Lynx
  6.6.2010
*/
#include <windows.h>
#include <assert.h>
#ifdef __cplusplus
extern "C" {
#endif
#include "cmd.h"
#include "xmpputil.h"
#ifdef __cplusplus
}
#endif
#include "win32funcs.h"

#ifdef _DEBUG
#define PLUGIN_NAME "win_d"
#else
#define PLUGIN_NAME "win"
#endif

#define EXPORT __declspec(dllexport)

#ifdef __cplusplus
extern "C" {
#endif

int EXPORT MsgBox (const Cmd *cmd, xmpp_ctx_t *ctx, 
				   xmpp_conn_t *const conn, xmpp_stanza_t *const stanza) {
    const ArgList *arg = cmd->args;
    assert(arg);
    MessageBox(NULL, arg->head.u.str, "OK", MB_OK);
    return 1;
}

int EXPORT ListProcess (const Cmd *cmd, xmpp_ctx_t *ctx,
						xmpp_conn_t *const conn, xmpp_stanza_t *const stanza) {
	const ArgList *arg = cmd->args;
	assert(arg);
	Win32::ProcessListType processList;
	Win32::GetProcessList(&processList, arg->head.u.num != 0);

    char replytext[2048];
    size_t pos = 0;
    for (Win32::ProcessListType::const_iterator it = processList.begin();
        it != processList.end() && pos < sizeof(replytext); ++it) {
            sprintf(&replytext[pos], "%d\t%s\n", it->id, it->name.c_str() );
            pos = strlen(replytext);
    }
    send_chattext(ctx, conn, stanza, replytext);
	return 1;
}

int EXPORT KillProcess (const Cmd *cmd, xmpp_ctx_t *ctx,
						xmpp_conn_t *const conn, xmpp_stanza_t *const stanza) { 
    const ArgList *arg = cmd->args;
    assert(arg);
    Win32::TerminateProcess(arg->head.u.num);
    return 1;
}

int EXPORT Shutdown (const Cmd *cmd, xmpp_ctx_t *ctx,
						xmpp_conn_t *const conn, xmpp_stanza_t *const stanza) { 
    const ArgList *arg = cmd->args;
    assert(arg);
    Win32::ShutdownSystem(arg->head.u.num != 0);
    return 1;
}

int EXPORT plugin_init (CmdState *cs) {
    cs_register(cs, PLUGIN_NAME, "msgbox", MsgBox);
    cs_register(cs, PLUGIN_NAME, "list_process", ListProcess);
    cs_register(cs, PLUGIN_NAME, "kill_process", KillProcess);
    cs_register(cs, PLUGIN_NAME, "shutdown", Shutdown);

    return 1;
}

void EXPORT plugin_deinit (CmdState *cs) {
    cs_unregisterall(cs, PLUGIN_NAME);
}

#ifdef __cplusplus
}
#endif
