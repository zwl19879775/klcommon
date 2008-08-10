/// 
/// @file kllua-binder.h
/// @author Kevin Lynx
/// @date 8.10.2008
///
#ifndef ___KL_LUA_BINDER_H_
#define ___KL_LUA_BINDER_H_

extern "C"
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

#include "kllua-typetraits.h"

namespace kl_common
{
	template <typename Prototype>
	class lua_binder;

	template <typename R>
	class lua_binder<R ()>
	{
	public:
		typedef R result_type;
		typedef result_type (*func_type)();
	public:
		static int lua_adapter( lua_State *L )
		{
			_func();
			return 1;
		}

	public:
		static func_type _func;
	};
	template <typename R>
	typename lua_binder<R ()>::func_type lua_binder<R ()>::_func = 0;

	template <typename R, typename P1>
	class lua_binder<R ( P1 )>
	{
	public:
		typedef R result_type;
		typedef result_type (*func_type)( P1 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P1 p1 = lua::param_traits<P1>::get_param( L, -1);
			result_type r = _func( p1 );
			lua::return_traits<result_type>::set_result( L, r );

			return 1;
		}

	public:
		static func_type _func;
	};
	template <typename R, typename P1>
	typename lua_binder<R ( P1 )>::func_type lua_binder<R ( P1 )>::_func = 0;

	template <typename binder_type>
	void lua_bind( lua_State *L, typename binder_type::func_type func, const char *name )
	{
		binder_type::_func = func;
		lua_pushcfunction( L, binder_type::lua_adapter );
		lua_setglobal( L, name );
	}
}

#endif // ___KL_LUA_BINDER_H_