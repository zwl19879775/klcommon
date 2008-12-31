/**
 * to test kl syntax tree 
 */
#include "kllex.h"
#include "klparser.h"
#include <stdio.h>
#include <stdarg.h>

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

void lex_error( struct lexState *ls, const char *format, ... )
{
	char buf[1024];
	va_list list;
	va_start( list, format );
	vsprintf( buf, format, list );
	va_end( list );
	
	fprintf( stderr, ">>lex error [#%u]: %s\n", ls->lineno, buf );
}

void test_syntax( const char *file )
{
	FILE *fp = fopen( file, "rb" );
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
	fread( buf, size, 1, fp );
	buf[size] = 0;
	fclose( fp );
	{
		struct lexState ls;
		struct treeNode *tree; 
		int t;
		lex_setinput( &ls, buf, lex_error );
		tree = syn_parse( &ls );
		print_tree( tree );
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

	test_syntax( argv[1] );
	return 0;
}
