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
	
	/// 
	/// You donot need use this class below, instead i suggest you use lua_bind function.
	/// Everytime you want to bind a function in lua, you should only do 2 steps :
	///   first, make a typedef of lua_binder;
	///   second, call lua_bind function with your typedefs.
	/// For example :
	/// Here you have a function which prototype is : int my_fn( const char * ), and now, you make 
	/// a typedef :
	///    typedef kl_common::lua_binder<int (const char*)> my_prototype;
	/// And then, you call lua_bind function to bind :
	///   kl_common::lua_bind( L, my_fn, "fn" ); which L is your lua_State, and fn is the function 
	/// name in lua script.
	///
	/// If you want your function to return more than one result to lua, you can archieve this by
	/// return a lua table, and you can extend lua::result_traits.
	///
	template <typename Prototype>
	class lua_binder;

#define RESULT_COUNT \
	lua::return_number_traits<result_type>::count

#define SET_RESULT( L, r ) \
	lua::return_traits<result_type>::set_result( L, r ); 

#define PARAM_TYPE( n ) typedef P##n p##n##_type
	///
	/// 0 parameter.
	///
	template <typename R>
	class lua_binder<R ()>
	{
	public:
		typedef R result_type;
		typedef result_type (*func_type)();
	public:
		///
		/// this function is an adapter function, lua script will call this function and 
		/// this function will call your function which will be binded.
		///
		static int lua_adapter( lua_State *L )
		{
			// when result_type is void, there's no result to return.
			caller<result_type>::call( L );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L )
			{
				result_type r = _func();
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L )
			{
				_func();
			}
		};

	public:
		static func_type _func;
	};
	template <typename R>
	typename lua_binder<R ()>::func_type lua_binder<R ()>::_func = 0;

	///
	/// 1 parameter.
	/// 
	template <typename R, typename P1>
	class lua_binder<R ( P1 )>
	{
	public:
		typedef R result_type;
		typedef result_type (*func_type)( P1 );
		PARAM_TYPE( 1 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P1 p1 = lua::param_traits<P1>::get_param( L, -1);
			
			caller<result_type>::call( L, p1 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1 )
			{
				result_type r = _func( p1 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1 )
			{
				_func( p1 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1>
	typename lua_binder<R ( P1 )>::func_type lua_binder<R ( P1 )>::_func = 0;

	///
	/// 2 parameters.
	/// 
	template <typename R, typename P1, typename P2>
	class lua_binder<R ( P1, P2 )>
	{
	public:
		typedef R result_type;
		typedef result_type (*func_type)( P1, P2 );
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P2 p2 = lua::param_traits<P2>::get_param( L, -1 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -2 );
			
			caller<result_type>::call( L, p1, p2 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2 )
			{
				result_type r = _func( p1, p2 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2 )
			{
				_func( p1, p2 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2>
	typename lua_binder<R ( P1, P2 )>::func_type lua_binder<R ( P1, P2 )>::_func = 0;

	///
	/// This function is the only interface to you, you can use it to bind any function to
	/// lua script. Currently it only support c-functions.Check lua_binder to see example.
	///
	/// @note you must specify the binder_type explicitly
	/// @param L lua_State, must be initialized already.
	/// @param func the function which will be binded.
	/// @param name the function name in lua script.
	///
	template <typename binder_type>
	void lua_bind( lua_State *L, typename binder_type::func_type func, const char *name )
	{
		binder_type::_func = func;
		lua_pushcfunction( L, binder_type::lua_adapter );
		lua_setglobal( L, name );
	}
}

#endif // ___KL_LUA_BINDER_H_