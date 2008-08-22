///
/// @file ls_script_holder.cpp
/// @author Kevin Lynx
/// @date 8.22.2008
/// @brief to hold script file data
///
#include "ls_script_holder.h"

namespace lua_sys
{
	void ScriptHolder::add_file( const std::string &name, std::size_t size, char *content )
	{
		FileInfo fi;
		fi._size = size;
		fi._content = content;
		_files[name] = fi;
	}

	void ScriptHolder::remove_file( const std::string &name )
	{
		FileList::iterator it = _files.find( name );
		if( it != _files.end() )
		{
			free( it->second._content );
			_files.erase( it );
		}
	}

	void ScriptHolder::clear()
	{
		for( FileList::iterator it = _files.begin(); it != _files.end(); ++ it )
		{
			free( it->second._content );
		}
		_files.clear();
	}
}
