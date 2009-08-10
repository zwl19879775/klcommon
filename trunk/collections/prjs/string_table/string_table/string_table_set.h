///
///
///
#ifndef ___STRING_TABLE_SET_H_
#define ___STRING_TABLE_SET_H_

#include <string>
#include <map>

namespace stable
{
	class string_table;
	class string_table_set
	{
	public:
		typedef unsigned long id_type;
		struct config
		{
			string_table *st;
			typedef std::map<id_type, std::string> FileTableT;
			FileTableT ft;
		};
		typedef std::map<id_type, config> TableSetT;

	public:
		~string_table_set()
		{
			clear();
		}

		bool load( const std::string &file );

		bool save();

		void clear();

		string_table *get_st( id_type id );
		
		string_table *get_key_st();

		void update_key_st_ids();
	private:
		TableSetT _tables;
		id_type _keyTable;
	};
}

#endif
