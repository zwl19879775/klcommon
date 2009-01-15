/**
 * @file kllibbase.c
 * @author Kevin Lynx
 * @brief to provide some basic functions in the script
 */
#include "kllib.h"
#include "kllibbase.h"
#include <stdio.h>
#include <malloc.h>
#include <string.h>

static struct TValue print( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	if( arg->type == NUMBER )
	{
		printf( "%lf", arg->dval );
	} else if( arg->type == STRING )
	{
		printf( arg->sval );
	}
	return ret;
}

static struct TValue printc( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	putc( (char)arg->dval, stdout );
	return ret;
}

static struct TValue input( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	if( arg->type == STRING )
	{
		if( strcmp( arg->sval, "%s" ) == 0 )
		{
			ret.sval = (char*) malloc( sizeof( char ) * 2056 ) ;
			gets( ret.sval );
			ret.type = STRING;
		}
		else if( strcmp( arg->sval, "%d" ) == 0 )
		{
			scanf( "%lf", &ret.dval );
			ret.type = NUMBER;
		}
		else if( strcmp( arg->sval, "%c" ) == 0 )
		{
			char c = getc( stdin );
		    ret.dval = c;
	   		ret.type = NUMBER;
		}		
	}
	return ret;
}

int kllib_open_base( struct klState *kl )
{
	kl_register( kl, print, "print" );
	kl_register( kl, printc, "putc" );
	kl_register( kl, input, "input" );
	return 0;
}

