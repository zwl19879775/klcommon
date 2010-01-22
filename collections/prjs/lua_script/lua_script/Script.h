///
///
///
#ifndef ___SCRIPT_H_
#define ___SCRIPT_H_

#include <list>
#include "utils/kl_cachedobj.h"

struct lua_State;

class Script : public kl_common::cached_obj<Script>
{
public:
	Script()
	{
		m_ID = 0;
		m_luaState = 0;
	}

	bool Init( long id, lua_State *mainState );

	int RunScript( const char *file );

	int RunStr( const char *str );

	int Resume();

	long ID() const
	{
		return m_ID;
	}

	lua_State *QueryState()
	{
		return m_luaState;
	}
private:
	long m_ID;
	lua_State *m_luaState;
};

#endif
