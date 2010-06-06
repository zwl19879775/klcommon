/**
 command functions
 Kevin Lynx
 6.5.2010
*/
#include "cmd.h"
#include <stdlib.h>
#include <string.h>

static void addarg (ArgList **args, Arg *arg) {
    if (*args == 0) {
        *args = (ArgList*) malloc(sizeof(**args));
        (*args)->head = *arg;
        (*args)->tail = 0;
    }
    else {
        ArgList *args_ = *args;
        for (; args_->tail; args_ = args_->tail) ;
        args_->tail = (ArgList*) malloc(sizeof(*args_->tail));
        args_->tail->head = *arg;
        args_->tail->tail = 0;
    }
}

static void freearg (ArgList *args) {
    while ( args ) {
        ArgList *t = args->tail;
        free(args);
        args = t;
    }
}

static void initcmd (Cmd *cmd) {
    cmd->plugin = 0;
    cmd->name = 0;
    cmd->args = 0;
}

static int tokmatch (Token *t, int tok) {
    if (t->t == tok) {
        return 0;
    }
    return -1;
}

static Cmd c_getcmd (const char *str) {
    ParseState ps;
    Cmd cmd;
    Token t;
    initcmd(&cmd);
    cp_init(&ps, str);
    /* plugin name */
    t = cp_token(&ps);
    if (tokmatch(&t, T_ID) < 0) return cmd;
    cmd.plugin = t.u.str;
    /* dot */
    t = cp_token(&ps);
    if (tokmatch(&t, T_DOT) < 0) return cmd;
    /* command function name */
    t = cp_token(&ps);
    if (tokmatch(&t, T_ID) < 0) return cmd;
    cmd.name = t.u.str;
    /* arg list */
    t = cp_token(&ps);
    while (t.t != T_EOF) {
        switch (t.t) {
            case T_NUM:
            case T_STRING:
                addarg(&cmd.args, &t);
                break;
            default:
                /* syntax error */
                break;
        }
        t = cp_token(&ps); /* ',' */
        if (t.t == T_EOF) break; /* no more args */
        if (tokmatch(&t, T_COMMA) < 0 ) {
            /* syntax error */
        }
        t = cp_token(&ps); /* next arg */
    }

    cp_deinit(&ps);
    return cmd;
}

static void c_freecmd (Cmd *cmd) {
    if (cmd->plugin) {
        free(cmd->plugin);
    }
    if (cmd->name) {
        free(cmd->name);
    }
    if (cmd->args) {
        freearg(cmd->args);
    }
}

/* windows dynamic library implementation */
#ifdef WIN32
#include <windows.h>

static void *loadlib (const char *file) {
    char s[MAX_PATH];
    HINSTANCE lib;
    strcpy(s, file);
    strcat(s, ".dll");
    lib = LoadLibraryA(s);
    return lib;
}

static void freelib (void *lib) {
    HINSTANCE inst = (HINSTANCE) lib;
    FreeLibrary(inst);
}

static void *getsymbol (void *lib, const char *name) {
    void *func = (void*) GetProcAddress((HINSTANCE) lib, name);
    return func;
}

#endif

static void freeplugin (Plugin *plugin) {
    sp_free(plugin->cmdfuncs);
    freelib(plugin->h);
    free(plugin);
}

static void tranversePlugins (StringPair *sp) {
    Plugin *p = (Plugin*) sp->u;
    freeplugin(p);
}

static CmdFunc getfunc (CmdState *cs, const char *plugin, const char *cmd) {
    Plugin *p = sp_get(cs->plugins, plugin);
    CmdFunc f = 0;
    if (!p) {
        return 0;
    }
    f = sp_get(p->cmdfuncs, cmd);
    return f;
}

static void addplugin (CmdState *cs, const char *name, Plugin *p) {
    sp_add(&cs->plugins, name, p);
}

CmdState *cs_new () {
    CmdState *cs = (CmdState*) malloc(sizeof(*cs));
    cs->plugins = 0;
    return cs;
}

void cs_free (CmdState *cs) {
    sp_tranvers(cs->plugins, tranversePlugins); 
    sp_free(cs->plugins);
    free(cs);
}

int cs_exe (CmdState *cs, const char *cmdstr, xmpp_ctx_t *ctx, 
            xmpp_conn_t *const conn ) {
    Cmd cmd = c_getcmd(cmdstr);
    CmdFunc f = 0;
    int ret;
    if (!cmd.name || !cmd.plugin) {
        c_freecmd(&cmd);
        return 0;
    }
    if (!cs_isloaded(cs, cmd.plugin) ) {
        /* not loaded, load the plugin */
        if (!cs_loadplugin(cs, cmd.plugin)) {
            return 0; /* load failed */
        }
    }
    f = getfunc(cs, cmd.plugin, cmd.name);
    ret = f(&cmd, ctx, conn);
    c_freecmd(&cmd);
    return ret;
}

int cs_register (CmdState *cs, const char *pluginname, const char *funcname, 
                 CmdFunc f) {
    Plugin *plugin = sp_get(cs->plugins, pluginname);
    if (!plugin) {
        return 0;
    }
    sp_add(&plugin->cmdfuncs, funcname, f);
    return 1;
}
int cs_loadplugin (CmdState *cs, const char *pluginfile) {
    Plugin *plugin = (Plugin*) malloc(sizeof(*plugin));
    plugin->cmdfuncs = 0;
    plugin->h = loadlib(pluginfile);
    if (plugin->h == 0) {
        free(plugin);
        return 0;
    }
    else {
        PluginInitFunc f = (PluginInitFunc) getsymbol(plugin->h, "plugin_init");
        if (f == 0) {
            freelib(plugin->h);
            free(plugin);
            return 0;
        }
        /* add the plugin */
        addplugin(cs, pluginfile, plugin);
        f(cs);
    }
    return 1;
}

void cs_unloadplugin (CmdState *cs, const char *pluginname) {
    Plugin *p = sp_get(cs->plugins, pluginname);
    if (p) {
        PluginDeInitFunc f = (PluginDeInitFunc) getsymbol(p->h, "plugin_deinit");
        if (f) {
            f(cs);
        }
        freeplugin(p);
        sp_remove(&cs->plugins, pluginname);
    }
}

int cs_isloaded (CmdState *cs, const char *pluginname) {
    Plugin *p = sp_get(cs->plugins, pluginname);
    return p ? 1 : 0;
}
