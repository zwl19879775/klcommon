/**
 * kl stand-alone interpreter
 * Kevin Lynx
 */
#include "kllib.h"
#include "kllibbase.h"
#include "kllibloader.h"
#include <stdio.h>
#include <stdarg.h>
#include <malloc.h>
#include <stdlib.h>

struct FileInfo
{
	char *buf;
	size_t size;
};

static void _log( size_t lineno, const char *format, ... )
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

static void print_usage( const char *program )
{
	fprintf( stderr, 
			"usage : %s [script].\n", program );
}

static int load_file( const char *filename, struct FileInfo *file_s )
{
	FILE *fp = fopen( filename, "r" );
	if( fp == 0 )
	{
		fprintf( stderr, "Cannot open %s\n", filename );
		return -1;
	}
	fseek( fp, 0, SEEK_END );
	file_s->size = ftell( fp );
	fseek( fp, 0, SEEK_SET );

	file_s->buf = (char*) malloc( file_s->size + 1 );
	file_s->size = fread( file_s->buf, sizeof( char), file_s->size, fp );
	file_s->buf[file_s->size] = 0;
	fclose( fp );

	return 0;
}

static void exec_file( struct FileInfo *file_s )
{
	struct klState *kl = kl_new( _log );
	kllib_open_base( kl );
	kllib_open_loader( kl );
	kl_run( kl, file_s->buf );	
	kl_free( kl );
}

int main( int argc, char **argv )
{
	printf( "kl script interpreter v%s 2009 by Kevin Lynx.\n",
		 kl_version() );
	if( argc < 2 )
	{
		print_usage( argv[0] );
		exit( -1 );
	}

	{
		struct FileInfo fs;
		if( load_file( argv[1], &fs ) == 0 )
		{
			exec_file( &fs );
			free( fs.buf );
		}
	}
	return 0;
}

