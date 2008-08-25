///
/// @file ls_script_holder.cpp
/// @author Kevin Lynx
/// @date 8.22.2008
/// @brief to hold script file data
///
#define _CRT_SECURE_NO_WARNINGS
#include "ls_script_holder.h"

#ifdef _DEBUG
#include <windows.h>
#endif

namespace lua_sys
{
	void ScriptHolder::add_file( const std::string &name, std::size_t size, char *content )
	{
		FileInfo *fi = new FileInfo();
		fi->_size = size;
		fi->_content = content;
		_files[name] = fi;
	}

	void ScriptHolder::remove_file( const std::string &name )
	{
		FileList::iterator it = _files.find( name );
		if( it != _files.end() )
		{
			free( it->second->_content );
			delete it->second ;
			_files.erase( it );
		}
	}

	void ScriptHolder::clear()
	{
		for( FileList::iterator it = _files.begin(); it != _files.end(); ++ it )
		{
			free( it->second->_content );
			delete it->second ;
		}
		_files.clear();
	}

#ifdef _DEBUG
	void ScriptHolder::dump() const
	{
		const char *path = "script_holder";
		CreateDirectory( path, NULL );
		int i = 0;
		char full_name[256];
		
		FILE *list_fp = fopen( "file_list.txt", "w" );
		fprintf( list_fp, "%u files\n", get_file_count() );

		for( FileList::const_iterator it = _files.begin(); it != _files.end(); ++ it )
		{
			sprintf( full_name, "%s\\%d.lua", path, i++ );
			FILE *fp = fopen( full_name, "wb" );
			fwrite( it->second->_content, it->second->_size, 1, fp );
			fclose( fp );

			fprintf( list_fp, "%s\n", it->first.c_str() );
		}

		fclose( list_fp );
	}
#endif
}
