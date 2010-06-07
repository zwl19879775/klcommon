/**
 command functions
 Kevin Lynx
 6.5.2010
*/
#ifndef ___CMD_H_
#define ___CMD_H_

#include "strophe.h"
#include "cparse.h"
#include "stringpair.h"

/* command argument list */
typedef Token Arg;
typedef struct _ArgList {
    Arg head; struct _ArgList *tail;
} ArgList;

typedef struct _Cmd {
    char *plugin; /* plugin name */
    char *name; /* command function name */
    ArgList *args;
} Cmd;

typedef struct _CmdState {
    StringPairList *plugins; /* <pluginname, plugin> */
} CmdState;

typedef struct _Plugin Plugin;
typedef int (*CmdFunc) (const Cmd*, xmpp_ctx_t*, xmpp_conn_t *const, 
						xmpp_stanza_t * const);
typedef int (*PluginInitFunc) (CmdState *cs);
typedef void (*PluginDeInitFunc) (CmdState *cs);

typedef StringPairList CmdFuncList;
typedef struct _Plugin {
    CmdFuncList *cmdfuncs; /* command function list */
    void *h; /* plugin handle */
    char d; /* dynamic plugin flag, 1:dynamic */
} Plugin;

CmdState *cs_new ();
void cs_free (CmdState *cs);

/**
  execute a command string.
*/
int cs_exe (CmdState *cs, const char *cmdstr, xmpp_ctx_t*, xmpp_conn_t*const, 
			xmpp_stanza_t * const);

/**
  register a command function in the CmdState
*/
int cs_register (CmdState *cs, const char *pluginname, const char *funcname,
                 CmdFunc f );

void cs_unregisterall (CmdState *cs, const char *pluginname);

/**
  add a plugin manually
*/
Plugin *cs_addplugin (CmdState *cs, const char *pluginname, char d);

/**
  only remove the plugin from the plugin list
*/
int cs_removeplugin (CmdState *cs, const char *pluginname);

/** 
  load a plugin
*/
int cs_loadplugin (CmdState *cs, const char *pluginfile);

/** 
  unload the plugin
*/
void cs_unloadplugin (CmdState *cs, const char *pluginname);

/**
  check whether the plugin has been loaded
*/
int cs_isloaded (CmdState *cs, const char *pluginname);

#endif
