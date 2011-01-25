/**
  Bind this blowfish implementation to lua
  Kevin Lynx
  1.25.2011
*/
#include <lua.hpp>
#include "blowfish.h"

#ifndef LUA_BLOWFISH_API
#define LUA_BLOWFISH_API __declspec(dllexport)
#endif
#ifdef _DEBUG
#include <stdio.h>
#define logd printf
#else
#define logd(fmt, ...)
#endif

#define BLOWFISH_NULL "blowfish pointer is null"

CBlowFish *check_blowfish(lua_State *L, int index)
{
    if(lua_islightuserdata(L, index))
    {
        return (CBlowFish*) lua_topointer(L, index);
    }
    logd("blowfish pointer is null.\n");
    return NULL;
}

int blowfish_create(lua_State *L)
{
    size_t count = 0;
    int size;
    const char *key = luaL_checklstring(L, 1, &count);
    size = luaL_checkint(L, 2);
    logd("blowfish_create, %s, %d.\n", key, size);
    CBlowFish *fish = new CBlowFish((const BYTE*) key, size);
    lua_pushlightuserdata(L, fish);
    return 1;
}

int blowfish_destroy(lua_State *L)
{
    CBlowFish *fish = check_blowfish(L, 1);
    if(!fish) return luaL_argerror(L, 1, BLOWFISH_NULL);
    logd("blowfish_destroy, %p.\n", fish);
    delete fish;
    return 0;
}

int blowfish_decrypt(lua_State *L)
{
    CBlowFish *fish = check_blowfish(L, 1);
    if(!fish) return luaL_argerror(L, 1, BLOWFISH_NULL);
    const char *d = luaL_checklstring(L, 2, NULL);
    int size = luaL_checkint(L, 3);
    char *decrypt = new char [size];
    size_t len = (size_t) fish->Decrypt((const BYTE*) d, (BYTE*) decrypt, (DWORD) size);
    logd("decrypt data(count=%d, len=%d): %s.\n", size, len, decrypt);
    lua_pushlstring(L, decrypt, len);
    delete [] decrypt;
    return 1;
}

int blowfish_encrypt(lua_State *L)
{
    CBlowFish *fish = check_blowfish(L, 1);
    if(!fish) return luaL_argerror(L, 1, BLOWFISH_NULL);
    size_t count;
    const char *d = luaL_checklstring(L, 2, &count);
    char *encrypt = new char [count + 8 - count%8 ];
    size_t len = (size_t) fish->Encrypt((const BYTE*) d, (BYTE*) encrypt, (DWORD) count);
    lua_pushlstring(L, encrypt, len);
    delete [] encrypt;
    return 1;
}

static luaL_reg funcs[] = {
    { "create", blowfish_create },
    { "destroy", blowfish_destroy },
    { "decrypt", blowfish_decrypt },
    { "encrypt", blowfish_encrypt },
    { NULL, NULL }
};

#ifdef __cplusplus
extern "C" {
#endif

LUA_BLOWFISH_API int luaopen_blowfish(lua_State *L)
{
    logd("open blowfish library.\n");
    luaL_openlib(L, "blowfish", funcs, 0);
    return 1;
}

#ifdef __cplusplus
}
#endif