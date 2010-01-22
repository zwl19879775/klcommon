///
///
///
#include "file_loader.h"
#include <stdio.h>
#include <stdlib.h>

static size_t FileSize( FILE *fp )
{
	fseek( fp, 0, SEEK_SET );
	fseek( fp, 0, SEEK_END );
	long size = ftell( fp );
	fseek( fp, 0, SEEK_SET );
	return (size_t) size;
}

void FileLoader::Destroy()
{
	if( _data.buf != 0 )
	{
		free( _data.buf );
		_data.buf = 0;
		_data.size = 0;
	}
}

bool FileLoader::Load( const char *file, int mode )
{
	const char *rmode = 0;
	if( mode == BINARY )
	{
		rmode = "rb";
	}
	else if( mode == TEXT )
	{
		rmode = "r";
	}
	if( rmode == 0 )
	{
		return false;
	}
	FILE *fp = fopen( file, rmode );
	if( fp == NULL )
	{
		return false;
	}
	_data.size = FileSize( fp );
	_data.buf = malloc( _data.size );
	fread( _data.buf, _data.size, 1, fp );
	fclose( fp );
	return true;
}
