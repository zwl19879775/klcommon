/**
 * @file 2dim.c
 * @author Kevin Lynx
 * @brief 2 dim array support
 */
#include "kllib.h"
#include <stdlib.h>
#include <memory.h>

#define CAST_PTR( dval, type ) (type)(long)(dval)

static struct TValue dim_new( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	double **array;
	size_t x = (size_t)kl_check_number( &arg );
	size_t y = (size_t)kl_check_number( &arg );
	array = (double**)malloc( sizeof( double* ) * x );
	if( array != 0 )
	{
		size_t i ;
		for( i = 0; i < x; ++ i )
		{
			array[i] = (double*)malloc( sizeof( double ) * y );
			if( array[i] != 0 )
			{
				memset( array[i], 0, sizeof( double ) * y );
			}
		}

		ret.dval = (long) array;
	}
	return ret;
}

static struct TValue dim_delete( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	double **array = CAST_PTR( kl_check_number( &arg ), double** );
	size_t x = (size_t)kl_check_number( &arg );
	size_t y = (size_t)kl_check_number( &arg );
	(void)y;
	size_t i ;
	for( i = 0; i < x; ++ i )
	{
		free( array[i] );
	}
	free( array );
	return ret;
}

static struct TValue dim_get( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	double **array = CAST_PTR( kl_check_number( &arg ), double** );
	if( array != 0 )
	{
		size_t x = (size_t)kl_check_number( &arg );
		size_t y = (size_t)kl_check_number( &arg );
		ret.dval = array[x][y];
	}	
	return ret;
}

static struct TValue dim_set( ArgType arg )
{
	DEF_DEFAULT_VAL( ret );
	double **array = CAST_PTR( kl_check_number( &arg ), double** );
	if( array != 0 )
	{
		size_t x = (size_t)kl_check_number( &arg );
		size_t y = (size_t)kl_check_number( &arg );
		double v = kl_check_number( &arg );
		array[x][y] = v;
	}	
	return ret;
}

void kllib_open_2dim( struct klState *kl )
{
	kl_register( kl, dim_new, "tdim_new" );
	kl_register( kl, dim_delete, "tdim_delete" );
	kl_register( kl, dim_get, "tdim_get" );
	kl_register( kl, dim_set, "tdim_set" );
}

void lib_open( struct klState *kl )
{
	kllib_open_2dim( kl );
}

