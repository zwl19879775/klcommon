///
/// @file kllua-typetraits.h
/// @author Kevin Lynx
/// @date 8.10.2008
///
#ifndef ___KL_LUA_TYPETRAITS_H_
#define ___KL_LUA_TYPETRAITS_H_

extern "C"
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

namespace kl_common
{
	namespace lua
	{
		template <typename _Tp>
		struct param_traits;

		template <>
		struct param_traits<int>
		{
			static int get_param( lua_State *L, int index )
			{
				return (int)lua_tonumber( L, index ); 
			}
		};

		template <>
		struct param_traits<const char*>
		{
			static const char *get_param( lua_State *L, int index )
			{
				return lua_tostring( L, index );
			}
		};

		template<>
		struct param_traits<void*>
		{
			static void *get_param( lua_State *L, int index )
			{
				return lua_touserdata( L, index );
			}
		};

		//////////////////////////////////////////////////////////////////////////////////////////////////
		template <typename _Tp>
		struct return_traits;

		template <>
		struct return_traits<int>
		{
			static void set_result( lua_State *L, int &r )
			{
				lua_pushnumber( L, r );
			}
		};

		template <>
		struct return_traits<const char*>
		{
			static void set_result( lua_State *L, const char *r )
			{
				lua_pushstring( L, r );
			}
		};
	}
}

#endif // end ___KL_LUA_TYPETRAITS_H_