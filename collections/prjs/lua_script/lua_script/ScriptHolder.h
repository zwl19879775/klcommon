///
///
///
#ifndef ___SCRIPT_HOLDER_H_
#define ___SCRIPT_HOLDER_H_

struct lua_State;

class ScriptHolder
{
public:
	ScriptHolder( lua_State *s )
	{
		m_luaState = s;
	}

	bool Init();

	bool Add( const char *file, const char *buf, size_t size );

	bool Get( const char *file );

	bool Get( const char *file, lua_State *L );

private:
	bool GetScriptTable( lua_State *L );
private:
	lua_State *m_luaState;
};

#endif
