///
///
///
#ifndef ___LS_SYSTEM_H_
#define ___LS_SYSTEM_H_

#include "ls_script_holder.h"
#include <string>

struct lua_State;

namespace lua_sys
{	
	///
	///
	///
	class System
	{
	public:
		///
		System();

		///
		~System();

		///
		bool initialize( ScriptHolder *holder );

		///
		void build();

		///
		void import( ScriptHolder::FileInfo &file );

		///
		void import( const std::string &script_name );

		///
		void release();

		///
		static System &instance();

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