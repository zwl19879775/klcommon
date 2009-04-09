/**
 * test kl interpreter
 */
#include "klinterpret.h"
#include "klparser.h"
#include "klsymtab.h"
#include "kllex.h"
#include <stdio.h>
#include <stdarg.h>
#include <malloc.h>
#include <stdlib.h>

int indentno = 0;

#define INDENT indentno+=2
#define UNINDENT indentno-=2

void print_space()
{
	int i = 0;
	for( i = 0; i < indentno; ++ i )
	{
		printf( " " );
	}
}

void print_tree( struct treeNode *node )
{
	int i;
	INDENT;
	while( node != 0 )
	{
		print_space();
		if( node->type == NT_STMT )
		{
			switch( node->subtype.stmt )
			{
				case ST_VAR_DEF:
					printf( "var def : %s\n", node->attr.val.sval );
					break;
				case ST_FUNC_DEF:
					printf( "func def : %s\n", node->attr.val.sval );
					break;
				case ST_PARAM_DEF:
					printf( "param def : %s\n", node->attr.val.sval );
					break;
				case ST_IF:
					printf( "if\n" );
					break;
				case ST_WHILE:
					printf( "while\n" );
					break;
				case ST_RETURN:
					printf( "return\n" );
					break;
				case ST_ARRAY_DEF:
					printf( "array def\n" );
					break;
				case ST_BREAK:
					printf( "break\n" );
					break;
				case ST_FOR:
					printf( "for\n" );
					break;
				default:
					printf( "unknown\n" );
			}
		}
		else if( node->type == NT_EXP )
		{
			switch( node->subtype.exp )
			{
				case ET_OP:
				   printf( "op : %c\n", node->attr.op );
				   break;	   
				case ET_CONST:
				   printf( "const : %lf\n", node->attr.val.dval );
				   break;
				case ET_STRING:
				   printf( "const string : %s\n", node->attr.val.sval );
				   break;
				case ET_ID:
				   printf( "id : %s\n", node->attr.val.sval );
				   break;
				case ET_ARRAY:
				   printf( "array : %s\n", node->attr.val.sval );
				   break;
				case ET_FUNC_CALL:
				   printf( "func call : %s\n", node->attr.val.sval );
				   break;
			}
		}
		else
		{
			/* error */
		}
		for( i = 0; i < MAXCHILDREN; ++ i )
		{
			print_tree( node->child[i] );
		}
		node = node->sibling;
	}
	UNINDENT;
}

void env_log( size_t lineno, const char *format, ... )
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

void test_inter( const char *file )
{
	FILE *fp = fopen( file, "r" );
	int size;
	char *buf;
	if( fp == 0 )
	{
		fprintf( stderr, "Cannot open %s\n", file );
		return;
	}
	fseek( fp, 0, SEEK_END );
	size = ftell( fp );
	fseek( fp, 0, SEEK_SET );
	
	buf = (char*) malloc( size + 1 );
	size = fread( buf, sizeof( char), size, fp );
	buf[size] = 0;
	fclose( fp );
	{
		struct lexState ls;
		struct treeNode *tree;
		struct interEnv *env = inter_env_new( env_log, 0 );
		lex_setinput( &ls, buf, env_log );
		tree = syn_parse( &ls );
		print_tree( tree );
		inter_build_global_st( env, tree );
		inter_execute( env );
		inter_env_free( env );
	}

	free( buf );
}

int main( int argc, char **argv )
{
	if( argc < 2 )
	{
		fprintf( stderr, "Usage : %s filename\n", argv[0] );
		exit( -1 );
	}

	test_inter( argv[1] );
	return 0;
}
