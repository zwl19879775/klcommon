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
#include "kl_macro_params.h"
#include "kl_static_typelist.h"

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
	///    typedef kl_common::lua_binder<int (const char*), 1> my_prototype;
	/// And then, you call lua_bind function to bind :
	///   kl_common::lua_bind( L, my_prototype::func_type( my_fn ), "fn" ); which L is your lua_State, and fn 
	/// is the function name in lua script. Whatever, the second arugment is a kl_common:functor.So you can 
	/// bind a member function if you wish.
	///
	/// If you want your function to return more than one result to lua, you can archieve this by
	/// return a lua table, and you can extend lua::result_traits.
	///
	/// 8.25.2008
	/// bug fix : And now, i add id to lua_binder, because these lua_binders which have the same prototype
	/// only have on _func member, so if you bind two functions with the same prototype, the second will
	/// overwrite the first one, and lua script will call wrong function. So i add an id parameter to partition
	/// these functions with the same prototype.
	///
	template <typename Prototype, long id>
	class lua_binder;

	///
	/// when id greater than BASE_YIELD_ID, these binded functions will yield lua thread.
	///
#define BASE_YIELD_ID (4096)

#define RESULT_COUNT \
	lua::return_number_traits<result_type>::count

#define SET_RESULT( L, r ) \
	lua::return_traits<result_type>::set_result( L, r ); 

#define STACK_INDEX( pi ) ( pi - param_count - 1 )
#define GET_PARAM( pi ) p##pi##_type p##pi = lua::param_traits<p##pi##_type>::get_param( L, STACK_INDEX( pi ) );
#define GET_PARAM_END p1_type p1 = lua::param_traits<p1_type>::get_param( L, -param_count );

#define DEF_GET_PARAM( n ) REPEAT_##n( n, GET_PARAM, GET_PARAM_END )


	///
	/// The macro template is to create lua_binder.( 1 -- n parameters )
	///
#define CREATE_LUA_BINDER( n ) \
	template <typename R, DEF_PARAM( n ), long id > \
	class lua_binder<R ( DEF_ARG( n ) ), id> \
	{ \
	public: \
		typedef R result_type; \
		typedef functor<result_type, STATIC_TYPE_LIST##n> func_type; \
		DEF_PARAM_TYPE( n ); \
		enum { param_count = n }; \
	public: \
		static int lua_adapter( lua_State *L ) \
		{ \
			DEF_GET_PARAM( n ) \
			caller<result_type>::call( L, DEF_FUNC_ARG( n ) ); \
			if( id > BASE_YIELD_ID ) \
				return ::lua_yield( L, RESULT_COUNT ); \
			else \
			return RESULT_COUNT; \
		} \
	private: \
		template <typename _Tp> \
		struct caller \
		{ \
			static void call( lua_State *L, DEF_FUNC_PARAM_REF( n ) ) \
			{ \
				result_type r = _func( DEF_FUNC_ARG( n ) ); \
				SET_RESULT( L, r ); \
			} \
		}; \
		template <> \
		struct caller<void> \
		{ \
			static void call( lua_State *L, DEF_FUNC_PARAM_REF( n ) ) \
			{ \
				_func( DEF_FUNC_ARG( n ) ); \
			} \
		}; \
	public: \
		static func_type _func; \
	}; \
	template <typename R, DEF_PARAM( n ), long id > \
	typename lua_binder<R ( DEF_ARG( n ) ), id>::func_type lua_binder<R ( DEF_ARG( n ) ), id>::_func 

	///
	/// 0 parameter.
	///
	template <typename R, long id>
	class lua_binder<R (), id>
	{
	public:
		typedef R result_type;
		typedef functor<result_type> func_type;
		enum { param_count = 0 };
	public:
		///
		/// this function is an adapter function, lua script will call this function and 
		/// this function will call your function which will be binded.
		///
		static int lua_adapter( lua_State *L )
		{
			// when result_type is void, there's no result to return.
			caller<result_type>::call( L );
			if( id > BASE_YIELD_ID )
				return ::lua_yield( L, 0 );
			else
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
	template <typename R, long id>
	typename lua_binder<R (), id>::func_type lua_binder<R (), id>::_func;

	CREATE_LUA_BINDER( 1 );
	CREATE_LUA_BINDER( 2 );
	CREATE_LUA_BINDER( 3 );
	CREATE_LUA_BINDER( 4 );
	CREATE_LUA_BINDER( 5 );
	CREATE_LUA_BINDER( 6 );
	CREATE_LUA_BINDER( 7 );
	CREATE_LUA_BINDER( 8 );
	CREATE_LUA_BINDER( 9 );
	CREATE_LUA_BINDER( 10 );
	CREATE_LUA_BINDER( 11 );
	CREATE_LUA_BINDER( 12 );
	CREATE_LUA_BINDER( 13 );
	CREATE_LUA_BINDER( 14 );
	CREATE_LUA_BINDER( 15 );

	///
	/// This function is the only interface to you, you can use it to bind any function to
	/// lua script. Currently it only support c-functions, operator(), and member functions.
	/// Check lua_binder to see example.
	///
	/// @note you must specify the binder_type explicitly
	/// @param L lua_State, must be initialized already.
	/// @param func the function wrapped by functor, which will be binded.
	/// @param name the function name in lua script.
	/// @param idx the module table index
	///
	template <typename binder_type>
	void lua_bind( lua_State *L, typename binder_type::func_type &func, const char *name, int idx = LUA_GLOBALSINDEX )
	{
		binder_type::_func = func;
		lua_pushcfunction( L, binder_type::lua_adapter );
		lua_setfield( L, idx, name );
	}

	///
	/// A much more comfortable wrap function to bind functions in lua. It's a wrapper for lua_bind.
	///
	/// i.e : lua_bind<void(int), 1>( L, func, "func" )
	///
	/// @param FuncType the binded function prototype.
	/// @param id for lua_binder id.
	/// @param BindType automatically generated by fn.
	///
	template <typename FuncType, long id, typename BindType>
	void lua_bind( lua_State *L, BindType fn, const char *name, int idx = LUA_GLOBALSINDEX )
	{
		typedef lua_binder< FuncType, id> binder_type;
		lua_bind<binder_type>( L, binder_type::func_type( fn ), name );
	}
}

#endif // ___KL_LUA_BINDER_H_