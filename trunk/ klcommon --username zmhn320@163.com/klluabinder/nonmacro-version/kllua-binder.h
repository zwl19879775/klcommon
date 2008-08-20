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
#include "kl_functor.h"

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
	///   kl_common::lua_bind( L, my_prototype::func_type( my_fn ), "fn" ); which L is your lua_State, and fn 
	/// is the function name in lua script. Whatever, the second arugment is a kl_common:functor.So you can 
	/// bind a member function if you wish.
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
		typedef functor<result_type> func_type;
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
	typename lua_binder<R ()>::func_type lua_binder<R ()>::_func;

	///
	/// 1 parameter.
	/// 
	template <typename R, typename P1>
	class lua_binder<R ( P1 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST1( P1 ) > func_type;
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
	typename lua_binder<R ( P1 )>::func_type lua_binder<R ( P1 )>::_func ;

	///
	/// 2 parameters.
	/// 
	template <typename R, typename P1, typename P2>
	class lua_binder<R ( P1, P2 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST2( P1, P2 ) > func_type;
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
	typename lua_binder<R ( P1, P2 )>::func_type lua_binder<R ( P1, P2 )>::_func ;

	///
	/// 3 parameters.
	/// 
	template <typename R, typename P1, typename P2, typename P3>
	class lua_binder<R ( P1, P2, P3 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST3( P1, P2, P3 ) > func_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P3 p3 = lua::param_traits<P3>::get_param( L, -1 );
			P2 p2 = lua::param_traits<P2>::get_param( L, -2 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -3 );
			
			caller<result_type>::call( L, p1, p2, p3 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3 )
			{
				result_type r = _func( p1, p2, p3 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3 )
			{
				_func( p1, p2, p3 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2, typename P3>
	typename lua_binder<R ( P1, P2, P3 )>::func_type lua_binder<R ( P1, P2, P3 )>::_func ;

	///
	/// 4 parameters.
	/// 
	template <typename R, typename P1, typename P2, typename P3, typename P4>
	class lua_binder<R ( P1, P2, P3, P4 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST4( P1, P2, P3, P4 ) > func_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P4 p4 = lua::param_traits<P4>::get_param( L, -1 );
			P3 p3 = lua::param_traits<P3>::get_param( L, -2 );
			P2 p2 = lua::param_traits<P2>::get_param( L, -3 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -4 );
			
			caller<result_type>::call( L, p1, p2, p3, p4 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4 )
			{
				result_type r = _func( p1, p2, p3, p4 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4 )
			{
				_func( p1, p2, p3, p4 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2, typename P3, typename P4>
	typename lua_binder<R ( P1, P2, P3, P4 )>::func_type lua_binder<R ( P1, P2, P3, P4 )>::_func ;

	///
	/// 5 parameters.
	/// 
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5>
	class lua_binder<R ( P1, P2, P3, P4, P5 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST5( P1, P2, P3, P4, P5 ) > func_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P5 p5 = lua::param_traits<P5>::get_param( L, -1 );
			P4 p4 = lua::param_traits<P4>::get_param( L, -2 );
			P3 p3 = lua::param_traits<P3>::get_param( L, -3 );
			P2 p2 = lua::param_traits<P2>::get_param( L, -4 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -5 );
			
			caller<result_type>::call( L, p1, p2, p3, p4, p5 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5 )
			{
				result_type r = _func( p1, p2, p3, p4, p5 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5 )
			{
				_func( p1, p2, p3, p4, p5 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5>
	typename lua_binder<R ( P1, P2, P3, P4, P5 )>::func_type lua_binder<R ( P1, P2, P3, P4, P5 )>::_func ;

	///
	/// 6 parameters.
	/// 
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6>
	class lua_binder<R ( P1, P2, P3, P4, P5, P6 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST6( P1, P2, P3, P4, P5, P6 ) > func_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
		PARAM_TYPE( 6 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P6 p6 = lua::param_traits<P6>::get_param( L, -1 );
			P5 p5 = lua::param_traits<P5>::get_param( L, -2 );
			P4 p4 = lua::param_traits<P4>::get_param( L, -3 );
			P3 p3 = lua::param_traits<P3>::get_param( L, -4 );
			P2 p2 = lua::param_traits<P2>::get_param( L, -5 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -6 );
			
			caller<result_type>::call( L, p1, p2, p3, p4, p5, p6 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6 )
			{
				result_type r = _func( p1, p2, p3, p4, p5, p6 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6 )
			{
				_func( p1, p2, p3, p4, p5, p6 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6>
	typename lua_binder<R ( P1, P2, P3, P4, P5, P6 )>::func_type lua_binder<R ( P1, P2, P3, P4, P5, P6 )>::_func ;

	///
	/// 7 parameters.
	/// 
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7>
	class lua_binder<R ( P1, P2, P3, P4, P5, P6, P7 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST7( P1, P2, P3, P4, P5, P6, P7 ) > func_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
		PARAM_TYPE( 6 );
		PARAM_TYPE( 7 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P7 p7 = lua::param_traits<P7>::get_param( L, -1 );
			P6 p6 = lua::param_traits<P6>::get_param( L, -2 );
			P5 p5 = lua::param_traits<P5>::get_param( L, -3 );
			P4 p4 = lua::param_traits<P4>::get_param( L, -4 );
			P3 p3 = lua::param_traits<P3>::get_param( L, -5 );
			P2 p2 = lua::param_traits<P2>::get_param( L, -6 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -7 );
			
			caller<result_type>::call( L, p1, p2, p3, p4, p5, p6, p7 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6, p7_type &p7 )
			{
				result_type r = _func( p1, p2, p3, p4, p5, p6, p7 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6, p7_type &p7 )
			{
				_func( p1, p2, p3, p4, p5, p6, p7 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7>
	typename lua_binder<R ( P1, P2, P3, P4, P5, P6, P7 )>::func_type lua_binder<R ( P1, P2, P3, P4, P5, P6, P7 )>::_func ;

	///
	/// 8 parameters.
	/// 
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8>
	class lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST8( P1, P2, P3, P4, P5, P6, P7, P8 ) > func_type;
		PARAM_TYPE( 1 );
		PARAM_TYPE( 2 );
		PARAM_TYPE( 3 );
		PARAM_TYPE( 4 );
		PARAM_TYPE( 5 );
		PARAM_TYPE( 6 );
		PARAM_TYPE( 7 );
		PARAM_TYPE( 8 );
	public:
		static int lua_adapter( lua_State *L )
		{
			P8 p8 = lua::param_traits<P8>::get_param( L, -1 );
			P7 p7 = lua::param_traits<P7>::get_param( L, -2 );
			P6 p6 = lua::param_traits<P6>::get_param( L, -3 );
			P5 p5 = lua::param_traits<P5>::get_param( L, -4 );
			P4 p4 = lua::param_traits<P4>::get_param( L, -5 );
			P3 p3 = lua::param_traits<P3>::get_param( L, -6 );
			P2 p2 = lua::param_traits<P2>::get_param( L, -7 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -8 );
			
			caller<result_type>::call( L, p1, p2, p3, p4, p5, p6, p7, p8 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6, p7_type &p7, p8_type &p8 )
			{
				result_type r = _func( p1, p2, p3, p4, p5, p6, p7, p8 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6, p7_type &p7, p8_type &p8 )
			{
				_func( p1, p2, p3, p4, p5, p6, p7, p8 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8>
	typename lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8 )>::func_type lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8 )>::_func ;

	///
	/// 9 parameters.
	/// 
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8, typename P9>
	class lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8, P9 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST9( P1, P2, P3, P4, P5, P6, P7, P8, P9 ) > func_type;
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
		static int lua_adapter( lua_State *L )
		{
			P9 p9 = lua::param_traits<P9>::get_param( L, -1 );
			P8 p8 = lua::param_traits<P8>::get_param( L, -2 );
			P7 p7 = lua::param_traits<P7>::get_param( L, -3 );
			P6 p6 = lua::param_traits<P6>::get_param( L, -4 );
			P5 p5 = lua::param_traits<P5>::get_param( L, -5 );
			P4 p4 = lua::param_traits<P4>::get_param( L, -6 );
			P3 p3 = lua::param_traits<P3>::get_param( L, -7 );
			P2 p2 = lua::param_traits<P2>::get_param( L, -8 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -9 );
			
			caller<result_type>::call( L, p1, p2, p3, p4, p5, p6, p7, p8, p9 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6, p7_type &p7, p8_type &p8, p9_type &p9 )
			{
				result_type r = _func( p1, p2, p3, p4, p5, p6, p7, p8, p9 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6, p7_type &p7, p8_type &p8, p9_type &p9 )
			{
				_func( p1, p2, p3, p4, p5, p6, p7, p8, p9 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8, typename P9>
	typename lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8, P9 )>::func_type lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8, P9 )>::_func ;

	///
	/// 10 parameters.
	/// 
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8, typename P9, typename P10>
	class lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 )>
	{
	public:
		typedef R result_type;
		typedef functor< result_type, TYPE_LIST10( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 ) > func_type;
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
		static int lua_adapter( lua_State *L )
		{
			P10 p10 = lua::param_traits<P10>::get_param( L, -1 );
			P9 p9 = lua::param_traits<P9>::get_param( L, -2 );
			P8 p8 = lua::param_traits<P8>::get_param( L, -3 );
			P7 p7 = lua::param_traits<P7>::get_param( L, -4 );
			P6 p6 = lua::param_traits<P6>::get_param( L, -5 );
			P5 p5 = lua::param_traits<P5>::get_param( L, -6 );
			P4 p4 = lua::param_traits<P4>::get_param( L, -7 );
			P3 p3 = lua::param_traits<P3>::get_param( L, -8 );
			P2 p2 = lua::param_traits<P2>::get_param( L, -9 );
			P1 p1 = lua::param_traits<P1>::get_param( L, -10 );
			
			caller<result_type>::call( L, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 );

			return RESULT_COUNT;
		}

	private:
		///
		/// to determine how to call _func, because the void return type.
		///
		template <typename _Tp>
		struct caller
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6, p7_type &p7, p8_type &p8, p9_type &p9, p10_type &p10 )
			{
				result_type r = _func( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 );
				SET_RESULT( L, r );
			}
		};

		///
		/// when result is void, i should only write '_func' code line.
		///
		template <>
		struct caller<void>
		{
			static void call( lua_State *L, p1_type &p1, p2_type &p2, p3_type &p3, p4_type &p4, p5_type &p5, p6_type &p6, p7_type &p7, p8_type &p8, p9_type &p9, p10_type &p10  )
			{
				_func( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 );
			}
		};

	public:
		static func_type _func;
	};
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5, typename P6, typename P7, typename P8, typename P9, typename P10>
	typename lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 )>::func_type lua_binder<R ( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 )>::_func ;

	///
	/// This function is the only interface to you, you can use it to bind any function to
	/// lua script. Currently it only support c-functions, operator(), and member functions.
	/// Check lua_binder to see example.
	///
	/// @note you must specify the binder_type explicitly
	/// @param L lua_State, must be initialized already.
	/// @param func the function wrapped by functor, which will be binded.
	/// @param name the function name in lua script.
	///
	template <typename binder_type>
	void lua_bind( lua_State *L, typename binder_type::func_type &func, const char *name )
	{
		binder_type::_func = func;
		lua_pushcfunction( L, binder_type::lua_adapter );
		lua_setglobal( L, name );
	}
}

#endif // ___KL_LUA_BINDER_H_