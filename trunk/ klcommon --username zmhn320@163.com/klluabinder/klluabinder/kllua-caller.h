///
/// @file kllua-caller.h
/// @author Kevin Lynx
/// @date 8.19.2008
///
#ifndef ___KL_LUA_CALLER_H
#define ___KL_LUA_CALLER_H

extern "C"
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

#include <string>
#include "kllua-typetraits.h"
#include "kl_macro_params.h"

namespace kl_common
{
	namespace lua
	{
		///
		/// The reason i wrap these lua api is to make more error checking when necessary.
		///
		inline void get_func( lua_State *L, const std::string &fn_name )
		{
			lua_getglobal( L, fn_name.c_str() );
		}

		inline void call_func( lua_State *L, int nargs, int nrets )
		{
			lua_pcall( L, nargs, nrets, 0 );
		}

		///
		/// Push results onto the stack
		///
		template <typename R>
		struct result
		{
			static R get( lua_State *L )
			{
				return CALLER_GET_RESULT( R, L );
			}
		};

		template <>
		struct result<void>
		{
			static void get( lua_State *L )
			{
			}
		};
	}

	///
	/// lua_caller makes you call lua script function more easy. It behavios like a functor.
	/// Usage :
	///   1. create a lua_caller object represents a lua function like :
	///		 kl_common::lua_caller<void(int)> fn( L, "s_fn" ); which 'void(int)' is the c prototype
	///      of the lua function, L is your lua_State pointer and s_fn is lua function name;
	///   2. call the lua function like :
	///      fn( 12 );
	///
	template <typename Prototype>
	class lua_caller;

#define RESULT_COUNT \
	lua::return_number_traits<result_type>::count

#define SET_PARAM( n ) ;CALLER_PUSH_PARAM( p##n##_type, _L, p##n )
#define SET_PARAM_END CALLER_PUSH_PARAM( p1_type, _L, p1 )
#define DEF_SET_PARAM( n ) REPEAT_##n( n, SET_PARAM, SET_PARAM_END )

	///
	/// The macro template is to create lua_caller.( 1 -- n parameters )
	///
#define CREATE_LUA_CALLER( n ) \
	template <typename R, DEF_PARAM( n ) > \
	class lua_caller<R ( DEF_ARG( n ) ) > \
	{ \
	public: \
		typedef R result_type; \
		DEF_PARAM_TYPE( n ); \
		enum { param_count = n }; \
	public: \
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name ) \
		{ \
		} \
		result_type operator() ( DEF_FUNC_PARAM( n ) ) \
		{ \
			lua::get_func( _L, _fn ); \
			DEF_SET_PARAM( n ); \
			lua::call_func( _L, param_count, RESULT_COUNT ); \
			return lua::result<result_type>::get( _L ); \
		} \
	private: \
		lua_State *_L; \
		const std::string _fn; \
	}

	///
	/// 0 parameter
	///
	template <typename R>
	class lua_caller<R ()>
	{
	public:
		typedef R result_type;
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ()
		{
			lua::get_func( _L, _fn );
			lua::call_func( _L, 0, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	CREATE_LUA_CALLER( 1 );
	CREATE_LUA_CALLER( 2 );
	CREATE_LUA_CALLER( 3 );
	CREATE_LUA_CALLER( 4 );
	CREATE_LUA_CALLER( 5 );
	CREATE_LUA_CALLER( 6 );
	CREATE_LUA_CALLER( 7 );
	CREATE_LUA_CALLER( 8 );
	CREATE_LUA_CALLER( 9 );
	CREATE_LUA_CALLER( 10 );
	CREATE_LUA_CALLER( 11 );
	CREATE_LUA_CALLER( 12 );
	CREATE_LUA_CALLER( 13 );
	CREATE_LUA_CALLER( 14 );
	CREATE_LUA_CALLER( 15 );

}

#endif // ___KL_LUA_CALLER_H