/**
 * test kl plugin
 */
#include "kllib.h"
#include <stdio.h>
#include <stdarg.h>

void log( struct klState *kl, const char *format, ... )
{
	char buf[1024];
	va_list list;
	va_start( list, format );
	vsprintf( buf, format, list );
	va_end( list );
	fprintf( stderr, buf );
}

struct TValue print( struct TValue *arg_list )
{
	struct TValue ret = { 0, NUMBER, 0 };
	if( arg_list->type == NUMBER )
	{
		printf( "%lf", arg_list->dval );
	} else if( arg_list->type == STRING )
	{
		printf( arg_list->sval );
	}
	return ret;
}

struct TValue printc( struct TValue *arg_list )
{
	struct TValue ret = { 0, NUMBER, 0 };
	putc( (char)arg_list->dval, stdout );
	return ret;
}

void test_plugin( const char *file )
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
	{
		struct klState *kl = kl_new( log );
		kl_register( kl, print, "print" );	
		kl_register( kl, printc, "putc" );
		kl_run( kl, buf );	
		kl_free( kl );
	}

	free( buf );
}

int main( int argc, char **argv )
{
	if( argc < 2 )
	{
		fprintf( stderr, "Usage : %s filename\n", argv[0] );
		exit( -1 );
	}

	test_plugin( argv[1] );
	return 0;
}

