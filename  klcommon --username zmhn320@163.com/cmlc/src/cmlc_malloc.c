/**
 * @file cmlc_malloc.c
 * @author Kevin Lynx
 * @brief the core of cmlc library, a tiny c memory leak checking library
 * @data 2.23.2008
 */
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cmlc_malloc.h"

#define INVALID_INDEX (size_t)(-1)

/* to disable malloc and free macro */
#ifdef malloc
#undef malloc
#endif
#ifdef free
#undef free
#endif

struct memRecord
{
	char file[MAX_FILE_NAME];
	char func[MAX_FUNC_NAME];
	size_t lineno;
	void *address;
	size_t size;
};

struct memory
{
	size_t index;
	void *address;
};

struct memRecord mem_record[MAX_RECORD];
size_t mr_tail;
size_t free_index[MAX_RECORD];
size_t fi_tail;

static int cmlc_can_record()
{
	if( mr_tail >= MAX_RECORD - 1 && fi_tail == 0 )
	{
		return 0;
	}
	else
	{
		return 1;
	}
}	

static size_t cmlc_get_free_index()
{
	if( fi_tail > 0 )
	{
		return free_index[fi_tail--];
	}
	if( mr_tail < MAX_RECORD )
	{
		return mr_tail++;
	}
	return INVALID_INDEX;
}

static int cmlc_insert_record( const char *file, const char *func, size_t lineno, void *address, size_t size )
{
	size_t index = cmlc_get_free_index();
	if( index == INVALID_INDEX )
	{
		return INVALID_INDEX;
	}

	mem_record[index].lineno = lineno;
	mem_record[index].address = address;
	mem_record[index].size = size;
	strncpy( mem_record[index].file, file, MAX_FILE_NAME - 1 );
	strncpy( mem_record[index].func, func, MAX_FUNC_NAME - 1 );

	return index;
}
	
static void cmlc_remove_record( size_t index )
{
	if( fi_tail < MAX_RECORD - 1 )
	{
		free_index[fi_tail++] = index;
		mem_record[index].address = 0; /* to identify this record has been removed */
	}
}

void *cmlc_malloc( size_t size, const char *file, const char *func, size_t lineno )
{
	struct memory *mem;
	if( !cmlc_can_record() )
	{
		return 0;
	}		
	
	mem = (struct memory*) malloc( size + sizeof( size_t ));
	mem->index = cmlc_insert_record( file, func, lineno, mem->address, size );
	mem->address = &mem->address;
	if( mem->index == INVALID_INDEX )
	{
		free( mem );
		return 0;
	}
	return mem->address;
}

void cmlc_free( void *address )
{
	struct memory *mem = (struct memory*)( (char*)address - sizeof( size_t ) );
	cmlc_remove_record( mem->index );
	free( mem );
}

static void cmlc_report()
{
	size_t i ;
	size_t num = 0;
	printf( "Memory leak report by cmlc, a tiny c memory leak checking library.\n" );
	for( i = 0; i < mr_tail; ++ i )
	{
		if( mem_record[i].address != 0 )
		{
			++ num;
			printf( "FILE: %s, FUNCTION: %s, LINE: %u, ADDRESS: 0x%p, SIZE: %u byte(s) leaks.\n", 
					mem_record[i].file, mem_record[i].func, 
					mem_record[i].lineno, mem_record[i].address, 
					mem_record[i].size );
		}
	}
	printf( "Detected %u memory leaks.\n", num ); 
}

void cmlc_init()
{
	mr_tail = 0;
	fi_tail = 0;
	atexit( cmlc_report );
}

