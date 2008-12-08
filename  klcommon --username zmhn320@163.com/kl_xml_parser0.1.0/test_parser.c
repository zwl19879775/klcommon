/**
  @file test_parser.c
*/
#include <stdio.h>
#include "xml_parser.h"

void error_log( unsigned long lineno, const char *msg )
{
	fprintf( stderr, "error at #%u : %s\n", lineno, msg );
}

void dump_attr( struct xmlAttr *attr )
{
	if( attr == 0 )
	{
		return;
	}
	printf( "ID:%s VALUE:%s\t", attr->name, attr->value );
	dump_attr( attr->next );
}

void dump_node( struct xmlNode *node )
{
	if( node == 0 )
	{
		return;
	}
	printf( "Node:%s Attr:", node->name );
	dump_attr( node->attr_list );
	putc( '\n', stdout );
	dump_node( node->children );
	dump_node( node->sibling );
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
		dump_node( doc->root );
		xmldoc_free( doc );
	}
	fclose( fp );

	return 0;
}
