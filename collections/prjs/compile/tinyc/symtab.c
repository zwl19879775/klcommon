/**
 * symtab.c
 * symbol table to store variables
 * Kevin Lynx
 * 4.4.2010
 */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define SYM_SIZE (256)

struct symbol
{
	char *name;
	int memloc;
};

static struct symbol syms[SYM_SIZE];
static int sym_pos = 0;

void sym_insert( const char *name, int loc )
{
	struct symbol sym;
	sym.name = strdup( name );
	sym.memloc = loc;
	syms[sym_pos++] = sym;
}

int sym_lookup( const char *name )
{
	int i;
	for( i = 0; i < sym_pos; ++ i )
	{
		if( strcmp( name, syms[i].name ) == 0 )
		{
			return syms[i].memloc;
		}
	}
	return -1;
}

void sym_clear()
{
	int i;
	for( i = 0; i < sym_pos; ++ i )
	{
		free( syms[i].name );
	}
}

void sym_dump()
{
	int i;
	printf( "name\tmemloc\n" );
	for( i = 0; i < sym_pos; ++ i )
	{
		printf( "%s\t%d\n", syms[i].name, syms[i].memloc );	
	}
}

