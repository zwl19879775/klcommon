///
///
///
#ifndef ___SCRIPT_SYS_H_
#define ___SCRIPT_SYS_H_

#include "utils/kl_singleton.h"
#include "ScriptHolder.h"
#include <map>

struct lua_State;
class Script;

/// singleton
class ScriptSys : public kl_common::singleton<ScriptSys>
{
public:
	typedef std::map<long, Script*> ScriptTableT;
public:
	ScriptSys()
	{
	}

	bool Init();

	void Release();

	int RunScript( const char *file );

	int RunStr( const char *script );

	int ResumeScript( long id );

	int ResumeScript( lua_State *L );

	long QueryID( lua_State *L );

	void BuildID( Script *s );

	ScriptHolder &GetHolder()
	{
		return *m_Holder;
	}

	Script *GetScript( long id );

	Script *GetScript( lua_State *L );
private:
	Script *NewScript();

	void DelScript( Script *s );

	void BuildFuncs();

	long GenID();

private:
	lua_State *m_MainState;
	ScriptHolder *m_Holder;
	ScriptTableT m_Scripts;
	long m_IDIndex;
};

#endif
