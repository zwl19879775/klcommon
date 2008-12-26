/**
 * to test kl syntax tree 
 */
#include "klparser.h"
#include <stdio.h>

void test_syntax( const char *file )
{
	FILE *fp = fopen( file, "rb" );
	int size;
	char *buf;
	if( fp == 0 )
	{
		fprintf( stderr, "Cannot open %s\n", file );
		return;
	}
	fseek( fp, 0, SEEK_END );
	size = ftell( fp );
	fseek( fp, 0, SEEK_SET );
	
	buf = (char*) malloc( size + 1 );
	fread( buf, size, 1, fp );
	buf[size] = 0;
	fclose( fp );



	free( buf );
}

int main( int argc, char **argv )
{
	if( argc < 2 )
	{
		fprintf( stderr, "Usage : %s filename\n", argv[0] );
		exit( -1 );
	}

	test_syntax( argv[1] );
	return 0;
}
