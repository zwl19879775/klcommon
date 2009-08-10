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
		typedef std::map<id_type, string_table*> TableSetT;

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

	private:
		TableSetT _tables;
		id_type _keyTable;
	};
}

#endif
