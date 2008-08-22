///
/// @file ls_loader.cpp
/// @author Kevin Lynx
/// @date 8.22.2008
/// @brief to provide some helper function like load scripts/encode scripts/decode scripts.
///
#include "ls_loader.h"
#include "ls_script_holder.h"

#ifdef __FLOWER_PUBLIC
#include "public.h"
#include "rfile.h"
#include "tools.h"
#include <deque>
#include <list>
using namespace std;
#include "ClientResource.h"


namespace lua_sys
{
	///
	/// Because there's not a function behaviors like this, so i write myself.
	///
	static std::size_t _GetStringFromByteArray( unsigned char *raw, std::size_t size, std::string &str )
	{
		std::size_t i;
		for( i = 0; i < size && raw[i] != 0; ++ i )
		{
			str.push_back( (char)raw[i] );
		}

		return i; 
	}

	std::size_t ScriptLoader::load( ScriptHolder &holder, const std::string &path, const std::string &suffix )
	{
		// get the file name list.
		typedef std::list<std::string> FileNameList;
		FileNameList file_names;
		FindFile( path.c_str(), suffix.c_str(), &file_names );
		// load them
		for( FileNameList::iterator it = file_names.begin(); it != file_names.end(); ++ it )
		{
			CRFile *rfile = rfOpen( it->c_str() );
			std::size_t size = rfile->GetDatalen();
			char *content = (char*) malloc( size );
			rfile->ReadData( content, (DWORD)size );
			rfClose( rfile );

			// save it
			holder.add_file( *it, size, content );
		}

		return file_names.size();
	}

	std::size_t ScriptLoader::encode( const ScriptHolder &holder, ScriptLoader::RawType &raw )
	{
		std::size_t ret = raw.size();
		// file list
		const ScriptHolder::FileList &fl = holder.get_file_list();
		// file count
		std::size_t count = holder.get_file_count();
		_AddToByteArray( &raw, (long) count );
	
		for( ScriptHolder::FileList::const_iterator it = fl.begin(); it != fl.end(); ++ it )
		{
			// name
			_AddToByteArray( &raw, it->first.c_str() );
			// file size
			_AddToByteArray( &raw, (long) it->second._size );
			// file content
			_AddToByteArray( &raw, it->second._content, (long) it->second._size );
		}

		return raw.size() - ret;
	}

	std::size_t ScriptLoader::decode( const ScriptLoader::RawType &raw, ScriptHolder &holder, std::size_t offset )
	{
		std::size_t init_off = offset;
		std::string name;
		std::size_t size;
		char *content;

		// file count
		long count = _GetLongFromByteArray( (unsigned char*) &raw[offset], (long&) offset );
		for( long i = 0; i < count; ++ i )
		{
			// name
			offset += _GetStringFromByteArray( (unsigned char*) &raw[offset], raw.size() - offset, name );
			// file size
			size = (std::size_t) _GetLongFromByteArray( (unsigned char*) &raw[offset], (long&)offset );
			// file content
			content = (char*) malloc( size );
			_GetBufferFromByteArray( (unsigned char*) &raw[offset], (long&)offset, content, (long)size );

			// and save it
			holder.add_file( name, size, content );
		}

		return offset - init_off;
	}
}

#endif
