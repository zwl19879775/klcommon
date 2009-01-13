/**
 * @file klsymtab.c
 * @author Kevin Lynx
 * @brief Symbol table implemention for kl.
 */
#include "klsymtab.h"
#include <stdio.h>

/* the hash function */
static int sym_hash( const char *key )
{
	int tmp = 0;
	int i = 0;
	for( i = 0; key[i] != '\0'; ++ i )
	{
		tmp = ((tmp << 4) + key[i] ) % SYM_SIZE;
	}
	return tmp;
}

struct symTable *sym_new()
{
	struct symTable *st = (struct symTable*) malloc( sizeof( struct symTable ) );
	memset( st->table, 0, SYM_SIZE * sizeof( st->table[0] ) );
	return st;
}

void sym_free( struct symTable *st )
{
	int i;
	for( i = 0; i < SYM_SIZE; ++ i )
	{
		struct Symbol *sl = st->table[i] ;
		for( ; sl != 0; sl = sl->next )
		{
			free( sl->name );
			if( sl->val.type == SB_VAR_STRING )
			{
				free( sl->val.sval );
			}
			else if( sl->val.type == SB_VAR_ARRAY )
			{
				size_t j;
				for( j = 0; j < sl->val.size; ++ j )
				{
					if( sl->val.aval[j].type == SB_VAR_STRING )
					{
						free( sl->val.aval[j].sval );
					}
				}
			}
		}
	}
	free( st );
}

int sym_insert( struct symTable *st, const char *name, struct Value val )
{
	int index = sym_hash( name );
	struct Symbol *head = st->table[index];
	struct Symbol *sb = sym_lookup( st, name );
	if( sb != 0 )
	{
		/* the symbol exists, update its value */
		sb->val = val;
	}
	else
	{
		/* create a new symbol and insert it */
		struct Symbol *new_sb = (struct Symbol*) malloc( sizeof( struct Symbol ) );
		new_sb->name = (char*) malloc( strlen( name ) + 1 );
		strcpy( new_sb->name, name );
		new_sb->val = val;
		new_sb->next = head;
		st->table[index] = new_sb;
	}

	return 0;
}	

struct Symbol *sym_lookup( struct symTable *st, const char *name )
{
	int index = sym_hash( name );
	struct Symbol *head = st->table[index];
	struct Symbol *sb;
	for( sb = head; sb != 0 && strcmp( sb->name, name ) != 0; sb = sb->next )
	{
	}
	return sb;
}

int sym_insert_array( struct symTable *st, const char *name, size_t size )
{
	struct Symbol *sb = sym_lookup( st, name );
	if( sb != 0 )
	{
		return -1;
	}
	else
	{
		int i;
		int index = sym_hash( name );
		struct Symbol *head = st->table[index];
		struct Symbol *new_sb = (struct Symbol*) malloc( sizeof( struct Symbol ) );
		new_sb->name = (char*) malloc( strlen( name ) + 1 );
		strcpy( new_sb->name, name );
		new_sb->val.type = SB_VAR_ARRAY;
		new_sb->val.size = size;
		new_sb->val.aval = (struct Value *) malloc( sizeof( struct Value ) * size );
		for( i = 0; i < size; ++ i )
		{
			new_sb->val.aval[i].type = SB_VAR_NUM;
			new_sb->val.aval[i].dval = 0;
		}
		new_sb->next = head;
		st->table[index] = new_sb;	
	}

	return 0;
}

struct Value *sym_lookup_array( struct symTable *st, const char *name, size_t index )
{
	return 0;
}
