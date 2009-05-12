/**
 * dnfsnd_extractor.c
 * to extract dnf sound resources
 *
 * Kevin Lynx
 * 5.12.2009
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *split_file_name( const char *full_name )
{
	size_t i, len = strlen( full_name );
	for( i = len - 1; i >= 0;  -- i )
	{
		if( full_name[i] == '/' )
		{
			return &full_name[i+1];
		}	
	}

	return "";
}

int extract_wav( const char *outfile, FILE *npk_file, long offset, long size )
{
	long pos = ftell( npk_file );
	char *buf; 
	FILE *fp;
	fp = fopen( outfile, "wb" );
	if( fp == 0 )
	{
		return -1;
	}
	buf = (char*) malloc( sizeof( char ) * size );
	fseek( npk_file, offset, SEEK_SET );
	fread( buf, sizeof( buf[0] ), size, npk_file );
	fseek( npk_file, pos, SEEK_SET );
	fwrite( buf, sizeof( buf[0] ), size, fp );
	fclose( fp );
	free( buf );
	return 0;
}

char *read_str( FILE *fp, char *buf, int rs )
{
	size_t size = fread( buf, sizeof( char ), rs, fp );
   	buf[size] = 0;
	return buf;	
}

void load_snd_npk( const char *npk_file )
{
	char snd_file[256];
	char header[16];
	long i, fc; /* file count */
	long offset, size;
	FILE *fp;

	fp = fopen( npk_file, "rb" );
	if( fp == 0 )
	{
		fprintf( stderr, "failed to open %s.\n", npk_file );
		return;
	}

	read_str( fp, header, 16 );
	fread( &fc, sizeof( fc ), 1, fp );
	printf( "%ld sounds in %s.\n", fc, npk_file );

	for( i = 0; i < fc; ++ i )
	{
		fread( &offset, sizeof( offset ), 1, fp );
		fread( &size, sizeof( size ), 1, fp );
		read_str( fp, snd_file, 256 );

		printf( "extract [%s]...", snd_file );
		if( extract_wav( split_file_name( snd_file ), fp, offset, size ) == 0 )
		{
			printf( "done.\n" );
		}
		else
		{
			printf( "failed.\n" );
		}
	}

	fclose( fp );
}

int main( int argc, char **argv )
{
	int i;
	if( argc < 2 )
	{
		fprintf( stderr, "Usage:%s npk1 npk2 np3...\n", argv[0] );
		exit( -1 );
	}
	for( i = 1; i < argc; ++ i )
	{
		load_snd_npk( argv[i] );
	}

	return 0;
}

		
