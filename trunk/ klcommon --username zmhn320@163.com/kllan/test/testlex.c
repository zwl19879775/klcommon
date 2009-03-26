/**
 * test kl lex
 */
#include <stdio.h>
#include "kllex.h"
#include <stdarg.h>
#include <malloc.h>

void lex_error( size_t lineno, const char *format, ... )
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

void test_lex( const char *file )
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
		int t;
		lex_setinput( &ls, buf, lex_error );

		for( t = lex_token( &ls ); ls.token.type != TK_EOF && ls.token.type != TK_ERROR; t = lex_token( &ls ) )
		{
			switch( t )
			{
				case TK_ID:
					printf( "ID : %s\n", lex_current_str( &ls ) );
					break;

				case TK_NUM:
				case TK_FLOAT:
					printf( "Number : %s\n", lex_current_str( &ls ) );
					break;

				case TK_CHAR:
					printf( "Char : %s\n", lex_current_str( &ls ) );
					break;

				case TK_STRING:
					printf( "String : %s\n", lex_current_str( &ls ) );
					break;

				case TK_IF:
					printf( "If\n" );
					break;

				case TK_ELSE:
					printf( "Else\n" );
					break;

				case TK_WHILE:
					printf( "While\n" );
					break;

				case TK_DO:
					printf( "Do\n" );
					break;

				case TK_RETURN:
					printf( "Return\n" );
					break;
					
				case TK_BREAK:
					printf( "Break\n" );
					break;

				case TK_FUNCTION:
					printf( "Function\n" );
					break;

				case TK_NE:
					printf( "!=\n" );
					break;

				case TK_EQ:
					printf( "==\n" );
					break;

				case TK_LE:
					printf( "<=\n" );
					break;

				case TK_GE:
					printf( ">=\n" );
					break;

				case TK_OR:
					printf( "||\n" );
					break;

				case TK_AND:
					printf( "&&\n" );
					break;

				default:
					printf( "%s\n", lex_current_str( &ls ) );
			}

			if( lex_current_str( &ls ) != 0 )
			{
				free( lex_current_str( &ls ) );
			}
		}
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

	test_lex( argv[1] );
	return 0;
}
