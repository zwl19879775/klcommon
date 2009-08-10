///
///
///
#ifndef ___STRING_TABLE_IMPL_H_
#define ___STRING_TABLE_IMPL_H_

#include "string_table.h"
#include "string_table_set.h"
#include "kl_singleton.h"

class StringTableImpl : public kl_common::singleton<StringTableImpl>,
	public stable::string_table_set
{
public:
	enum LangType
	{
		LT_TW = 1,
		LT_SIM = 2,
	};
	enum StrType
	{
		ST_RGN = 1,
		ST_GOODS = 2,
	};
public:
	///
	/// @param type ST_RGN etc
	///
	const std::string &GetSimByTw( long type, const std::string &str )
	{
		stable::string_table *st_tw = get_key_st();
		id_type id = st_tw->query_id( type, str );
		stable::string_table *st_sim = get_st( LT_SIM );
		const std::string &s = st_sim->get( type, id );
		if( s == stable::string_table::null_string )
		{
			return str;
		}
		return s;
	}
};

#ifndef SUPPORT_TW
#define ST_CONVERT( type, str ) \
	StringTableImpl::getSingleton().GetSimByTw( type, str )
#else
#define ST_CONVERT( type, str ) str
#endif

#define ENABLE_COL
#ifdef ENABLE_COL
#define ST_COLLECT( type, str ) \
	{ \
		stable::string_table *st = StringTableImpl::getSingleton().get_key_st(); \
		st->add( type, str ); st->update_id_table(); \
	}
#else
#define ST_COLLECT( type, str ) 
#endif
#endif
