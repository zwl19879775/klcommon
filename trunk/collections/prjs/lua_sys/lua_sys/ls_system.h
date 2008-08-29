///
/// @file ls_system.h
/// @author Kevin Lynx
/// @brief the interface wrapped the lua system.
///
#ifndef ___LS_SYSTEM_H_
#define ___LS_SYSTEM_H_

#include "ls_script_holder.h"
#include <string>

struct lua_State;

namespace lua_sys
{	
	///
	/// System is a singleton that you can access it by System::instance().
	///
	class System
	{
	public:
		/// trivial ctor
		System();

		/// trivial dtor
		~System();

		///
		/// initialize the lua script system
		/// including building the lua state and register some functions to lua.
		///
		/// @param holder a ScriptHolder which holds lua scripts.
		///
		bool initialize( ScriptHolder *holder );

		///
		/// build the game scripe entironment, before you call this, ensure the 
		/// ScriptHolder has already hold scripts.
		///
		void build();

		///
		/// import a script file into lua entironment.
		///
		void import( ScriptHolder::FileInfo &file );

		///
		/// this function will search FileInfo by its name.
		///
		void import( const std::string &script_name );

		///
		/// close the lua state
		///
		void release();

		///
		/// query the System instance.
		///
		static System &instance();

		///
		/// query the lua_State
		///
		lua_State *query_state() 
		{
			return _L;
		}

	private:
		lua_State *_L;	
		ScriptHolder *_holder;
	};
}

#endif // ___LS_SYSTEM_H_