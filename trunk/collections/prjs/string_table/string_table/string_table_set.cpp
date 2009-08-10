///
///
///
#include "string_table_set.h"
#include "string_table.h"
#include "file_loader.h"
#include "xml/tinyxml.h"

namespace stable
{
	template <typename _Tp>
	_Tp query_attr( TiXmlElement *node, const char *name )
	{
		double v = 0;
		node->Attribute( name, &v );
		return (_Tp) v;
	}

	static string_table_set::id_type read_lang_cfg( TiXmlElement *node, 
		string_table_set::config &cfg )
	{
		string_table_set::id_type type = query_attr<string_table_set::id_type>(
			node, "type" );
		TiXmlElement *str_node = node->FirstChildElement( "Strings" );
		while( str_node != 0 )
		{
			int value = query_attr<int>( str_node, "value" );
			std::string file = str_node->Attribute( "file" );
			cfg.ft.insert( std::make_pair( value, file ) );
			str_node = str_node->NextSiblingElement();
		}

		cfg.st = new string_table();
		return type;
	}

	bool string_table_set::load( const std::string &file )
	{
		FileLoader loader;
		if( !loader.Load( file.c_str(), FileLoader::TEXT ) )
		{
			return false;
		}
	
		FileLoader::RawData &raw = loader.GetRawData();
		TiXmlDocument doc;
		if( !doc.Parse( (char*) raw.buf ) )
		{
			return false;
		}
		TiXmlElement *root = doc.RootElement();
		TiXmlElement *node = root->FirstChildElement();
		_keyTable = query_attr<id_type>( node, "key" ); 	
		// string tables.
		TiXmlElement *st_node = node->FirstChildElement( "Language" );
		id_type type;
		while( st_node != 0 )
		{
			config cfg;
			type = read_lang_cfg( st_node, cfg );	
			// save it.
			_tables.insert( std::make_pair( type, cfg ) );
			// load the string table.
			for( config::FileTableT::iterator it = cfg.ft.begin();
				it != cfg.ft.end(); ++ it )
			{
				cfg.st->load( it->first, it->second );
			}
			st_node = st_node->NextSiblingElement();
		}

		return true;
	}

	bool string_table_set::save()
	{
		for( TableSetT::iterator it = _tables.begin();
			it != _tables.end(); ++ it )
		{
			config &cfg = it->second;
			for( config::FileTableT::iterator sit = cfg.ft.begin();
				sit != cfg.ft.end(); ++ sit )
			{
				cfg.st->save( sit->first, sit->second );
			}
		}
		return true;
	}

	void string_table_set::clear()
	{
		for( TableSetT::iterator it = _tables.begin();
			it != _tables.end(); ++ it )
		{
			delete it->second.st;
		}
		_tables.clear();
	}

	string_table *string_table_set::get_st( id_type id )
	{
		TableSetT::iterator it = _tables.find( id );
		return it == _tables.end() ? NULL : it->second.st;
	}

	string_table *string_table_set::get_key_st()
	{
		return get_st( _keyTable );
	}

	void string_table_set::update_key_st_ids()
	{
		string_table *st = get_key_st();
		if( st != NULL )
		{
			st->update_id_table();
		}
	}
}
