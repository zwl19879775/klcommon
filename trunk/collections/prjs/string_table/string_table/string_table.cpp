///
///
///
#include "string_table.h"
#include "file_loader.h"
#include <sstream>

namespace stable
{
	const std::string string_table::null_string;

	bool string_table::load( id_type type, const std::string &file )
	{
		StringsT *strs = get_strings( type );
		if( strs != NULL )
		{
			return false;
		}
		strs = new_strings( type );
		FileLoader loader;
		if( !loader.Load( file.c_str(), FileLoader::TEXT ) )
		{
			return false;
		}
		FileLoader::RawData &raw = loader.GetRawData();
		std::stringstream stream( (char*) raw.buf );
		id_type id;
		std::string str;
		while( !stream.eof() )
		{
			void *ret = stream >> id >> str;
			if( ret == NULL )
			{
				break;
			}
			strs->insert( std::make_pair( id, str ) );
		}
		return true;
	}

	bool string_table::save( id_type type, const std::string &file )
	{
		StringsT *strs = get_strings( type );
		if( strs == NULL )
		{
			return false;
		}

		FILE *fp = fopen( file.c_str(), "w" );
		if( fp == NULL )
		{
			return false;
		}
		for( StringsT::const_iterator it = strs->begin(); it != strs->end();
			++ it )
		{
			fprintf( fp, "%u\n%s\n", it->first, it->second.c_str() );
		}
		fclose( fp );
		return true;
	}

	void string_table::clear()
	{
		for( StringsTableT::iterator it = _stable.begin(); it != _stable.end();
			++ it )
		{
			delete it->second;
		}
		_stable.clear();
		for( IDListTableT::iterator it = _ids.begin(); it != _ids.end();
			++ it )
		{
			delete it->second;
		}
		_ids.clear();
	}

	bool string_table::add( id_type type, id_type id, const std::string &str )
	{
		StringsT *strs = get_strings( type );
		if( strs == NULL )
		{
			return false;
		}
		for( StringsT::iterator it = strs->begin(); it != strs->end(); ++ it )
		{
			if( it->second == str )
			{
				return false;
			}
		}
		strs->insert( std::make_pair( id, str ) );
		return true;
	}

	void string_table::build_id_table()
	{
		for( StringsTableT::iterator it = _stable.begin(); it != _stable.end(); ++ it )
		{
			StringsT *strs = it->second;
			IDListT *id_list = new IDListT();
			for( StringsT::iterator sit = strs->begin(); sit != strs->end(); ++ sit )
			{
				id_list->insert( std::make_pair( sit->second, sit->first ) );
			}
			_ids.insert( std::make_pair( it->first, id_list ) );
		}
	}

	string_table::id_type string_table::query_id( id_type type, const std::string &str )
	{
		IDListTableT::const_iterator it = _ids.find( type );
		if( it == _ids.end() )
		{
			return 0;
		}
		IDListT::const_iterator id_it = it->second->find( str );
		return id_it == it->second->end() ? 0 : id_it->second;
	}

	const std::string &string_table::get( id_type type, id_type id ) const
	{
		const StringsT *strs = get_strings( type );
		if( strs == NULL )
		{
			return null_string;
		}
		StringsT::const_iterator it = strs->find( id );
		return it == strs->end() ? null_string : it->second;
	}

	size_t string_table::size( id_type type ) const
	{
		const StringsT *strs = get_strings( type );
		if( strs == NULL )
		{
			return 0;
		}
		return strs->size();
	}

	string_table::StringsT *string_table::get_strings( id_type type )
	{
		StringsTableT::iterator it = _stable.find( type );
		return it == _stable.end() ? NULL : it->second;
	}

	const string_table::StringsT *string_table::get_strings( id_type type ) const
	{
		StringsTableT::const_iterator it = _stable.find( type );
		return it == _stable.end() ? NULL : it->second;
	}

	string_table::StringsT *string_table::new_strings( id_type type )
	{
		StringsT *strs = new StringsT();
		_stable.insert( std::make_pair( type, strs ) );
		return strs;
	}
}
