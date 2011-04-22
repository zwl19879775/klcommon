/*
脚本模块增强功能需求
----------------------

* C函数可接收Lua匿名、局部函数对象
  Lua不支持C端获取函数对象，无论是匿名函数、局部函数、甚至是全局函数。
  可在Lua层做封装，用以支持这种特性：callback.lua

* 程序返回某个简化对象，脚本可容易访问该对象的属性，及设置属性: accessor.lua


*/
#include <lua.hpp>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct triger {
	char fn_name[128];
	char arg1[128];
	char arg2[128];
};

triger *create_triger (const char *fn_name, const char *s1, const char *s2) {
	triger *t = (triger*) malloc(sizeof(triger));
	strcpy(t->fn_name, fn_name);
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
	const char *fn_name = luaL_checkstring(L, -3);
	gt = create_triger(fn_name, s1, s2);
	return 0;
}

void call_triger (triger *t, lua_State *L) {
	lua_getglobal(L, t->fn_name);
	lua_pushstring(L, t->arg1);
	lua_pushstring(L, t->arg2);
	lua_call(L, 2, 0);
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
	system("pause");
	call_triger(gt, L);
	lua_close(L);
	return 0;
}
