/**
  windows plugin 
  Kevin Lynx
  6.6.2010
*/
#include <windows.h>
#include <assert.h>
#include "cmd.h"

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
                                  xmpp_conn_t *const conn) {
    const ArgList *arg = cmd->args;
    assert(arg);
    MessageBox(NULL, arg->head.u.str, "OK", MB_OK);
    return 1;
}

int EXPORT plugin_init (CmdState *cs) {
    cs_register(cs, PLUGIN_NAME, "msgbox", MsgBox);
    return 1;
}

void EXPORT plugin_deinit (CmdState *cs) {
}

#ifdef __cplusplus
}
#endif
