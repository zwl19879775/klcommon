/**
  to test xml_helper 
*/
#include <stdio.h>
#include "xml_helper.h"

void error_log( unsigned long lineno, const char *msg )
{
	fprintf( stderr, "error at #%u : %s\n", lineno, msg );
}

int main( int argc, char **argv )
{
	FILE *fp = 0;
	if( argc < 2 )
	{
		fprintf( stderr, "Usage : %s filename.\n", argv[0] );
		exit( -1 );
	}
	fp = fopen( argv[1], "r" );
	if( fp == 0 )
	{
		fprintf( stderr, "Unable to open %s.\n", argv[1] );
		exit( -1 );
	}
	{
		struct xmlDocument *doc = xmldoc_new( fp );
		xml_seterrorfn( error_log );
		xml_parse( doc );
		{
			/* test these functions/macros provided by xml_helper */
			struct xmlNode *root = xml_root( doc );
			struct xmlNode *node = xml_getchild( root, "Child" );
			int id;
			xml_attrint( node, "id", &id );
			printf( "value : %d\n", id );
		}
		xmldoc_free( doc );
	}
	fclose( fp );
	return 0;
}
