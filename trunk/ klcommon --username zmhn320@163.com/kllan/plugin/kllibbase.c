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
	if( kl_is_number( arg ) )
	{
		printf( "%lf", kl_check_number( &arg ) );
	} else if( kl_is_string( arg ) )
	{
		printf( kl_check_string( &arg ) );
	}
	return ret;
}

static struct TValue printc( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	putc( (char)kl_check_number( &arg ), stdout );
	return ret;
}

static struct TValue input( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	if( kl_is_string( arg ) )
	{
		const char *fmt = kl_check_string( &arg );
		if( strcmp( fmt, "%s" ) == 0 )
		{
			ret.sval = (char*) malloc( sizeof( char ) * 2056 ) ;
			gets( ret.sval );
			ret.type = STRING;
		}
		else if( strcmp( fmt, "%d" ) == 0 )
		{
			scanf( "%lf", &ret.dval );
			ret.type = NUMBER;
		}
		else if( strcmp( fmt, "%c" ) == 0 )
		{
			char c = getc( stdin );
		    ret.dval = c;
	   		ret.type = NUMBER;
		}		
	}
	return ret;
}

static struct TValue bitor( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	long l = (long) kl_check_number( &arg );
	long r = (long) kl_check_number( &arg );
	ret.dval = ( l | r );
	return ret;
}

static struct TValue bitand( ArgType arg )
{
	struct TValue ret = { { 0 }, NUMBER, 0 };
	long l = (long) kl_check_number( &arg );
	long r = (long) kl_check_number( &arg );
	ret.dval = ( l & r );
	return ret;
}

static struct TValue bitnot( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	long o = (long) kl_check_number( &arg );
	ret.dval = (~o);
	return ret;
}

int kllib_open_base( struct klState *kl )
{
	kl_register( kl, print, "print" );
	kl_register( kl, printc, "putc" );
	kl_register( kl, input, "input" );
	kl_register( kl, bitor, "bit_or" );
	kl_register( kl, bitand, "bit_and" );
	kl_register( kl, bitnot, "bit_not" );
	return 0;
}

