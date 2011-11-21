///
/// @file xl.cpp
/// @author Kevin Lynx
/// @brief A jabberbot plugin which use Xunlei to download files
/// @date 11.21.2011
///
#include <windows.h>
#include <tchar.h>
#include "XLDownload.h"
#include "XLError.h"
#ifdef __cplusplus
extern "C" {
#endif
#include "cmd.h"
#include "xmpputil.h"
#ifdef __cplusplus
}
#endif

#ifdef _DEBUG
#define PLUGIN_NAME "xl_d"
#else
#define PLUGIN_NAME "xl"
#endif

#define EXPORT __declspec(dllexport)

// implementations
#include <vector>

typedef std::vector<LONG> TaskList;

// all tasks, including these finished tasks.
static TaskList s_tasks;

static wchar_t *WChar(const char *str) {
	wchar_t *wstr;
	int size = MultiByteToWideChar(CP_ACP, NULL, str, (int)strlen(str), NULL, 0);
	wstr = new wchar_t[size+1];
	wcsset(reinterpret_cast<wchar_t*>(wstr), 0);
	MultiByteToWideChar(CP_ACP, NULL, str, strlen(str), (LPWSTR)wstr, size);
	wstr[size] = 0;
	return wstr;
}

static void StopAllTasks() {
    for (TaskList::iterator it = s_tasks.begin(); it != s_tasks.end(); ++it) {
        XLStopTask(*it);
    }
    s_tasks.clear();
}

#ifdef __cplusplus
extern "C" {
#endif

int EXPORT Download(const Cmd *cmd, xmpp_ctx_t *ctx, xmpp_conn_t *const conn, 
                    xmpp_stanza_t *const stanza) {
    const ArgList *args = cmd->args;
    const char *url = ARG_STR(args);
    const char *file = ARG_NEXT(args) ? ARG_STR(args) : NULL;
	if (!url || !file)
		return 0;
    LONG taskId = 0;
    DWORD ret = XLURLDownloadToFile(WChar(file), WChar(url), _T(""), taskId);
	if (ret == XL_SUCCESS)
	{
		send_formattext(ctx, conn, stanza, "start download success, task id: %d", taskId);
	}
	else
	{
		send_formattext(ctx, conn, stanza, "start download failed, error: %u", ret);
	}
    return 1;
}

int EXPORT plugin_init(CmdState *cs) {
    if (XLInitDownloadEngine())
        return 0;
    cs_register(cs, PLUGIN_NAME, "download", Download);
    return 1;
}

void EXPORT plugin_deinit(CmdState *cs) {
    StopAllTasks();
    XLUninitDownloadEngine();
    cs_unregisterall(cs, PLUGIN_NAME);
}

#ifdef __cplusplus
}
#endif

