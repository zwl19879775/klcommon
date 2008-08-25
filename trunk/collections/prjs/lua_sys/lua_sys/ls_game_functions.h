///
///
///
#ifndef ___LS_GAME_FUNCTIONS_H_
#define ___LS_GAME_FUNCTIONS_H_

struct lua_State;

///
/// open flower game library for lua.
///
int luaopen_game( lua_State *L );

///
/// lua exeception handler
///
int lua_error_handler( lua_State *L );

#endif // ___LS_GAME_FUNCTIONS_H_ 