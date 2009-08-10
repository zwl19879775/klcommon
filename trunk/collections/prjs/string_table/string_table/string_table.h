///
///
///
#ifndef ___STRING_TABLE_H_
#define ___STRING_TABLE_H_

#include <string>
#include <map>

namespace stable
{
	class string_table
	{
	public:
		typedef unsigned long id_type;
		typedef std::map<id_type, std::string> StringsT;
		typedef std::map<id_type, StringsT*> StringsTableT;
		typedef std::map<std::string, id_type> IDListT;
		typedef std::map<id_type, IDListT*> IDListTableT;
	public:
		~string_table()
		{
			clear();
		}

		bool load( id_type type, const std::string &file );

		bool save( id_type type, const std::string &file );

		void clear();

		void update_id_table();

		bool add( id_type type, const std::string &str );

		id_type query_id( id_type type, const std::string &str );

		const std::string &get( id_type type, id_type id ) const;

		size_t size( id_type type ) const;

	private:
		id_type alloc_id( const StringsT *st );

		StringsT *get_strings( id_type type );

		const StringsT *get_strings( id_type type ) const;

		StringsT *new_strings( id_type type );
	public:
		static const std::string null_string;
	private:
		StringsTableT _stable;
		IDListTableT _ids;
	};
}

#endif
