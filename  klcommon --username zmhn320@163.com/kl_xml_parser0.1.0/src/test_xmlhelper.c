/**
  to test xml_helper 
*/
#include <stdio.h>
#include <stdlib.h>
#include "xml_helper.h"

void error_log( unsigned long lineno, const char *msg )
{
	fprintf( stderr, "error at #%u : %s\n", lineno, msg );
}

int main( int argc, char **argv )
{
	FILE *fp = 0;
	void *buf;
	size_t size;
	if( argc < 2 )
	{
		fprintf( stderr, "Usage : %s filename.\n", argv[0] );
		exit( -1 );
	}
	fp = fopen( argv[1], "r" );
#ifndef XML_USE_FILE
	fseek( fp, 0, SEEK_END );
	size = ftell( fp );
	fseek( fp, 0, SEEK_SET );
	buf = malloc( size );
	fread( buf, size, 1, fp );
#endif	
	if( fp == 0 )
	{
		fprintf( stderr, "Unable to open %s.\n", argv[1] );
		exit( -1 );
	}
	{
#ifdef XML_USE_FILE
		struct xmlDocument *doc = xmldoc_new( fp );
#else
		struct xmlDocument *doc = xmldoc_new( buf, size );
		free( buf );
#endif
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
