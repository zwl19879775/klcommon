/**
 * symbol table for type_check practice
 * kevin lynx
 * 3.26.2010
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sym_table.h"

struct Type
{
	char *desc; /* string description */
};

/* the type table */
static struct Type type_t[MAX_TYPE];
/* type table position */
static int type_t_pos = 0;

/* register a type in the type table */
int type_register( const char *desc )
{
	struct Type type;
	if( type_t_pos >= MAX_TYPE )
	{
		return -1;
	}
	type.desc = (char*) malloc( strlen( desc ) + 1 );
	strcpy( type.desc, desc );
	type_t[type_t_pos++] = type;
	return type_t_pos - 1;
}

/* get the type id from its description */
int type_get_id( const char *desc )
{
	int i = 0;
	for( ; i < type_t_pos; ++ i )
	{
		if( strcmp( type_t[i].desc, desc ) == 0 )
		{
			return i;
		}
	}
	return -1;
}

/* get the type desc from its id */
const char *type_get_desc( int id )
{
	if( id < 0 || id >= type_t_pos )
	{
		return 0;
	}
	return type_t[id].desc;
}

/* register basic type in the type table */
static void type_register_base()
{
	type_register( "char" );
	type_register( "short" );
	type_register( "int" );
	type_register( "float" );
	type_register( "double" );
}

void type_init()
{
	type_register_base();
}

void type_release()
{
	while( type_t_pos > 0 )
	{
		free( type_t[--type_t_pos].desc );
	}
}

void type_dump()
{
	int i = 0;
	printf( "id\tdesc\n" );
	for( ; i < type_t_pos; ++ i )
	{
		printf( "%d\t%s\n", i, type_t[i].desc );
	}
}

/*****************************************************************************/
struct Symbol
{
	char *name;
	int type; /* symbol type */
	/* add more properties */
};

static struct Symbol sym_t[MAX_SYM];
static int sym_t_pos = 0;

/* add a symbol in the table if it does not exist */
int sym_add( const char *name, int type )
{
	struct Symbol sym;
	if( sym_t_pos >= MAX_SYM )
	{
		return -1;
	}
	sym.name = (char*) malloc( strlen( name ) + 1 );
	strcpy( sym.name, name );
	sym.type = type;
	sym_t[sym_t_pos++] = sym;
	return sym_t_pos - 1;
}

/* get a symbol's type */
int sym_type( const char *name )
{
	int i = 0;
	for( ; i < sym_t_pos; ++ i )
	{
		if( strcmp( sym_t[i].name, name ) == 0 )
		{
			return sym_t[i].type;
		}
	}
	return -1;
}

/* clear the symbol table */
void sym_clear()
{
	while( sym_t_pos > 0 )
	{
		free( sym_t[--sym_t_pos].name );
	}
}

void sym_dump()
{
	int i = 0;
	printf( "name\ttype\n" );
	for( ; i < sym_t_pos; ++ i )
	{
		const char *desc = type_get_desc( sym_t[i].type );
		printf( "%s\t%s\n", sym_t[i].name, desc );
	}
}

