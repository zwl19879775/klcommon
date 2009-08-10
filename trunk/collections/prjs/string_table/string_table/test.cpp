///
///
///
#include "string_table_impl.h"
#include <stdio.h>

int main()
{
	StringTableImpl st;
	st.load( "st_set.xml" );
	ST_COLLECT( StringTableImpl::ST_RGN, "水晶湖畔" );
	ST_COLLECT( StringTableImpl::ST_RGN, "水晶村" );
	ST_COLLECT( StringTableImpl::ST_RGN, "史X城" );

	printf( "%s\n", ST_CONVERT( StringTableImpl::ST_RGN, "水晶村" ).c_str() );
	st.save();
	return 0;
}
