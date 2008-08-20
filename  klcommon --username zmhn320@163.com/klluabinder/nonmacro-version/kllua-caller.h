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

	#define PARAM_TYPE( n ) typedef P##n p##n##_type
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

	///
	/// 1 parameter
	///
	template <typename R, typename P1>
	class lua_caller<R ( P1 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			lua::call_func( _L, 1, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 2 parameters
	///
	template <typename R, typename P1, typename P2>
	class lua_caller<R ( P1, P2 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			lua::call_func( _L, 2, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 3 parameters
	///
	template <typename R, typename P1, typename P2, typename P3>
	class lua_caller<R ( P1, P2, P3 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2, p3_type p3 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			CALLER_PUSH_PARAM( p3_type, _L, p3 );
			lua::call_func( _L, 3, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 4 parameters
	///
	template <typename R, typename P1, typename P2, typename P3, typename P4>
	class lua_caller<R ( P1, P2, P3, P4 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2, p3_type p3, p4_type p4 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			CALLER_PUSH_PARAM( p3_type, _L, p3 );
			CALLER_PUSH_PARAM( p4_type, _L, p4 );
			lua::call_func( _L, 4, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 5 parameters
	///
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5>
	class lua_caller<R ( P1, P2, P3, P4, P5 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2, p3_type p3, p4_type p4, p5_type p5 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			CALLER_PUSH_PARAM( p3_type, _L, p3 );
			CALLER_PUSH_PARAM( p4_type, _L, p4 );
			CALLER_PUSH_PARAM( p5_type, _L, p5 );
			lua::call_func( _L, 5, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 6 parameters
	///
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6>
	class lua_caller<R ( P1, P2, P3, P4, P5, P6 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
		PARAM_TYPE( 6 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2, p3_type p3, p4_type p4, p5_type p5, p6_type p6 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			CALLER_PUSH_PARAM( p3_type, _L, p3 );
			CALLER_PUSH_PARAM( p4_type, _L, p4 );
			CALLER_PUSH_PARAM( p5_type, _L, p5 );
			CALLER_PUSH_PARAM( p6_type, _L, p6 );
			lua::call_func( _L, 6, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 7 parameters
	///
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7>
	class lua_caller<R ( P1, P2, P3, P4, P5, P6, P7 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
		PARAM_TYPE( 6 );
		PARAM_TYPE( 7 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2, p3_type p3, p4_type p4, p5_type p5, p6_type p6, p7_type p7 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			CALLER_PUSH_PARAM( p3_type, _L, p3 );
			CALLER_PUSH_PARAM( p4_type, _L, p4 );
			CALLER_PUSH_PARAM( p5_type, _L, p5 );
			CALLER_PUSH_PARAM( p6_type, _L, p6 );
			CALLER_PUSH_PARAM( p7_type, _L, p7 );
			lua::call_func( _L, 7, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 8 parameters
	///
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8>
	class lua_caller<R ( P1, P2, P3, P4, P5, P6, P7, P8 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
		PARAM_TYPE( 6 );
		PARAM_TYPE( 7 );
		PARAM_TYPE( 8 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2, p3_type p3, p4_type p4, p5_type p5, p6_type p6, p7_type p7, p8_type p8 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			CALLER_PUSH_PARAM( p3_type, _L, p3 );
			CALLER_PUSH_PARAM( p4_type, _L, p4 );
			CALLER_PUSH_PARAM( p5_type, _L, p5 );
			CALLER_PUSH_PARAM( p6_type, _L, p6 );
			CALLER_PUSH_PARAM( p7_type, _L, p7 );
			CALLER_PUSH_PARAM( p8_type, _L, p8 );
			lua::call_func( _L, 8, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 9 parameters
	///
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8, typename P9>
	class lua_caller<R ( P1, P2, P3, P4, P5, P6, P7, P8, P9 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
		PARAM_TYPE( 6 );
		PARAM_TYPE( 7 );
		PARAM_TYPE( 8 );
		PARAM_TYPE( 9 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2, p3_type p3, p4_type p4, p5_type p5, p6_type p6, p7_type p7, p8_type p8, p9_type p9 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			CALLER_PUSH_PARAM( p3_type, _L, p3 );
			CALLER_PUSH_PARAM( p4_type, _L, p4 );
			CALLER_PUSH_PARAM( p5_type, _L, p5 );
			CALLER_PUSH_PARAM( p6_type, _L, p6 );
			CALLER_PUSH_PARAM( p7_type, _L, p7 );
			CALLER_PUSH_PARAM( p8_type, _L, p8 );
			CALLER_PUSH_PARAM( p9_type, _L, p9 );
			lua::call_func( _L, 9, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};

	///
	/// 10 parameters
	///
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8, typename P9, typename P10>
	class lua_caller<R ( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 )>
	{
	public:
		typedef R result_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
		PARAM_TYPE( 6 );
		PARAM_TYPE( 7 );
		PARAM_TYPE( 8 );
		PARAM_TYPE( 9 );
		PARAM_TYPE( 10 );
	public:
		lua_caller( lua_State *L, const std::string &fn_name ) : _L( L ), _fn( fn_name )
		{
		}

		result_type operator() ( p1_type p1, p2_type p2, p3_type p3, p4_type p4, p5_type p5, p6_type p6, p7_type p7, p8_type p8, p9_type p9, p10_type p10 )
		{
			lua::get_func( _L, _fn );
			CALLER_PUSH_PARAM( p1_type, _L, p1 );
			CALLER_PUSH_PARAM( p2_type, _L, p2 );
			CALLER_PUSH_PARAM( p3_type, _L, p3 );
			CALLER_PUSH_PARAM( p4_type, _L, p4 );
			CALLER_PUSH_PARAM( p5_type, _L, p5 );
			CALLER_PUSH_PARAM( p6_type, _L, p6 );
			CALLER_PUSH_PARAM( p7_type, _L, p7 );
			CALLER_PUSH_PARAM( p8_type, _L, p8 );
			CALLER_PUSH_PARAM( p9_type, _L, p9 );
			CALLER_PUSH_PARAM( p10_type, _L, p10 );
			lua::call_func( _L, 10, RESULT_COUNT );
			return lua::result<result_type>::get( _L );
		}
	private:
		lua_State *_L;
		const std::string _fn;
	};
}

#endif // ___KL_LUA_CALLER_H