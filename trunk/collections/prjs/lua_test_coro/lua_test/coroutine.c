extern "C"
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

enum Request { getLoginRequest, getPasswordRequest };
const Request getLoginToken = getLoginRequest;
const Request getPasswordToken = getPasswordRequest;

int getLogin(lua_State *luaState)
{
	lua_pushlightuserdata(luaState, (void *)&getLoginToken);
	/* Return 1 value directly to C */
	return lua_yield(luaState, 1);
}

int getPassword(lua_State *luaState)
{
	lua_pushlightuserdata(luaState, (void *)&getPasswordToken);
	/* Return 1 value directly to C */
	return lua_yield(luaState, 1);
}

const char luaScript[] = 
"print('Lua')\n"
"login = getLogin()\n"
"password = getPassword()\n"
"print('login:', login, ' password:', password)\n";


int main(int argc, char *argv[])
{
	lua_State *luaState;
	int result;
	Request *token;
	const char *response = NULL;
	const char login[] = "root";
	const char password[] = "admin";
	
	luaState = lua_open();
	luaopen_base(luaState);
	lua_register(luaState, "getLogin", getLogin);
	lua_register(luaState, "getPassword", getPassword);

	/* Compile the script */
	if((result = luaL_loadbuffer(luaState, luaScript, sizeof(luaScript)-1, "luaScript")) != 0)
	{
		fprintf(stderr, lua_tostring(luaState, -1));
		return result;
	}

	/* Start the thread */
	lua_resume(luaState, 0);

	while (lua_isuserdata(luaState, 1))
	{
		printf("C\n");
		/* Get the yielded value from the script */
		token = (Request *)lua_touserdata(luaState, 1);
		lua_pop(luaState, 1);
		switch(*token)
		{
			case getLoginRequest : response = login;	break;
			case getPasswordRequest : response = password; break;
		}
		/* Return 1 string as the result of the yielding function */
		lua_pushstring(luaState, response);
		lua_resume(luaState, 1);
	}

	lua_close(luaState);
	return 0;
}

/**
Lua
C
C
login: root password: admin
*/