/**
 * test kl plugin
 */
#include "kllib.h"
#include "kllibbase.h"
#include <stdio.h>
#include <stdarg.h>
#include <malloc.h>
#include "klmemcheck.h"

void _log( size_t lineno, const char *format, ... )
{
	char buf[1024];
	va_list list;
	va_start( list, format );
	vsprintf( buf, format, list );
	va_end( list );
	fprintf( stderr, "# %u ", lineno ); 
	fprintf( stderr, buf );
	fprintf( stderr, "\n" );
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
		struct klState *kl = kl_new( _log );
		kllib_open_base( kl );
		kl_run( kl, buf );	
		{
			/* test call script function */
			struct TValue ret;
			ArgType arg_list;
			kl_new_arg( arg_list );
			kl_add_number( arg_list, 1 );
			kl_add_string( arg_list, "a string from c" );
			ret = kl_call( kl, "func", arg_list );
			printf( "\nfunc returns %lf\n", ret.dval );
			kl_free_arg( arg_list );
		}
		kl_free( kl );
	}

	free( buf );
}

int main( int argc, char **argv )
{
#ifdef _CHECK_MEMORY_LEAK
	duma_init();
#endif
	if( argc < 2 )
	{
		fprintf( stderr, "Usage : %s filename\n", argv[0] );
		exit( -1 );
	}

	test_plugin( argv[1] );
	return 0;
}

