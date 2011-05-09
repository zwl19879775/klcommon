///
/// Implement callback in Lua by luaL_ref.
/// Kevin Lynx
/// 5.9.2011
///
 
#include <lua5.1/lua.hpp>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG_S printf

struct triger {
    int callback;
	char arg1[128];
	char arg2[128];
};

triger *create_triger (int ref, const char *s1, const char *s2) {
	triger *t = (triger*) malloc(sizeof(triger));
    t->callback = ref;
	strcpy(t->arg1, s1);
	strcpy(t->arg2, s2);
	return t;
}

void delete_triger (triger *t) {
	free(t);
}

triger *gt;

int test_fn (lua_State *L) {
	const char *s2 = luaL_checkstring(L, -1);
	const char *s1 = luaL_checkstring(L, -2);
    /* pop the 2 arguments */
    lua_pop (L, 2); 
    DEBUG_S ("push callback: %s\n", luaL_typename (L, -1));
    /* store the callback function in registry */
    int fn_ref = luaL_ref (L, LUA_REGISTRYINDEX);
    DEBUG_S ("fn: %d, p1: %s, p2: %s\n", fn_ref, s1, s2);
	gt = create_triger(fn_ref, s1, s2);
	return 0;
}

void call_triger (triger *t, lua_State *L) {
    /* get the callback function */
    lua_rawgeti (L, LUA_REGISTRYINDEX, t->callback);
    DEBUG_S ("callback: %s\n", luaL_typename (L, -1));
	lua_pushstring(L, t->arg1);
	lua_pushstring(L, t->arg2);
	lua_pcall(L, 2, 0, 0);
    /* remove the callback function */
    luaL_unref (L, LUA_REGISTRYINDEX, t->callback);
	delete_triger(t);
}

void register_func (lua_State *L) {
	lua_pushcfunction(L, test_fn);
	lua_setglobal(L, "register_fn");
}

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "Usage: %s lua-script\n", argv[0]);
		return -1;
	}
	lua_State *L = lua_open();
	luaL_openlibs(L);
	register_func(L);
	printf("execute %s\n", argv[1]);
	if (luaL_loadfile(L, argv[1]) != 0 || lua_pcall(L, 0, 0, 0)) {
		fprintf(stderr, "%s\n", lua_tostring(L, -1));
		return -1;
	}
    getchar();
	call_triger(gt, L);
	lua_close(L);
	return 0;
}
