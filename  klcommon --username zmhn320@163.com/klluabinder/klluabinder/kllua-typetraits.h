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
	///
	/// These components in lua namespace is only used internally.
	///
	/// But if you want to extend some other customer types, you can access this namespace.
	/// And i also suggest you do that.
	///
	namespace lua
	{
		///
		/// param_traits can get the arguments from lua script.
		///
		/// The default template is to get all numbers : int, long , char, double etc...
		///
		template <typename _Tp>
		struct param_traits
		{
			static _Tp get_param( lua_State *L, int index )
			{
				return static_cast<_Tp>( lua_tonumber( L, index ) );
			}
		};

		///
		/// strings
		/// @note const char* is different from char*.
		///
		template <>
		struct param_traits<const char*>
		{
			static const char *get_param( lua_State *L, int index )
			{
				return lua_tostring( L, index );
			}
		};

		///
		/// all generic user data.
		///
		template <typename _Tp>
		struct param_traits<_Tp*>
		{
			static _Tp* get_param( lua_State *L, int index )
			{
				return static_cast<_Tp*>( lua_touserdata( L, index ) );
			}
		};

		///
		/// booleans.
		///
		template <>
		struct param_traits<bool>
		{
			static bool get_param( lua_State *L, int index )
			{
				return lua_toboolean( L, index ) != 0 ? true : false;
			}
		};

		///
		/// all generic pointers(in lua concept).
		///
		template <typename _Tp>
		struct param_traits<const _Tp*>
		{
			static const _Tp* get_param( lua_State *L, int index )
			{
				return static_cast<const _Tp*>( lua_topointer( L, index ) );
			}
		};

		///
		/// return_traits can push c-function return results onto the lua stack.
		///
		/// The default accepts all lua_Numbers.
		///
		template <typename _Tp>
		struct return_traits
		{
			static void set_result( lua_State *L, lua_Number r )
			{
				lua_pushnumber( L, r );				
			}
		};

		///
		/// user data
		///
		template <typename _Tp>
		struct return_traits<_Tp*>
		{
			static void set_result( lua_State *L, _Tp *r )
			{
				lua_pushlightuserdata( L, r );
			}
		};

		///
		/// const user data
		///
		template <typename _Tp>
		struct return_traits<const _Tp*>
		{
			static void set_result( lua_State *L, const _Tp *r )
			{
				lua_pushlightuserdata( L, const_cast<_Tp*>( r ) );
			}
		};

		///
		/// strings
		///
		template <>
		struct return_traits<const char*>
		{
			static void set_result( lua_State *L, const char *r )
			{
				lua_pushstring( L, r );
			}
		};

		///
		/// i suppose it's better consider char* to be a string.
		///
		template <>
		struct return_traits<char*>
		{
			static void set_result( lua_State *L, char *r )
			{
				lua_pushstring( L, r );
			}
		};

		///
		/// booleans
		///
		template <>
		struct return_traits<bool>
		{
			static void set_result( lua_State *L, bool r )
			{
				lua_pushboolean( L, r ? 1 : 0 );
			}
		};

		///
		/// return_number_traits can determine how many results to return to lua script.
		/// In fact, it only determine the number is 0 or 1.
		///
		template <typename _Tp>
		struct return_number_traits
		{
			/// the default is 1.
			enum
			{
				count = 1
			};
		};

		/// 
		/// no return values.
		///
		template <>
		struct return_number_traits<void>
		{
			enum
			{
				count = 0
			};
		};

		///
		/// these codes below is used for calling lua function from c++.
		///
		/// Because these codes is similar with lua_binder, so i #define them to make everything simple.
		/// I'm lazy to write these codes which i already wrote.
		///
		
		/// 
		/// push arguments onto lua stack.
		///
		#define CALLER_PUSH_PARAM( type, L, p ) ::kl_common::lua::return_traits<type>::set_result( L, p )

		///
		/// get return value from lua function.
		///
		#define CALLER_GET_RESULT( type, L ) ::kl_common::lua::param_traits<type>::get_param( L, -1 )
	}
}

#endif // end ___KL_LUA_TYPETRAITS_H_