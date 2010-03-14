/**
 * kevin lynx
 * 2.14.2010
 */
#include <stdio.h>
#include "scan.h"

const char *buf;
char lex_value;
int lex_type;

static char get_c()
{
	return *buf++;
}

static int is_done()
{
	return *buf == '\0';
}

void scan_init( const char *str )
{
	lex_value = 0;
	lex_type = NONE;
	buf = str;
}

void scan_reverse()
{
	buf--;
}

int scan_next()
{
	lex_value = 0;	
	lex_type = NONE;
	while( !is_done() )
	{
		char c = get_c();
		switch( c )
		{
			case '\t':
			case '\r':
			case '\n':
			case ' ':
				break;

			case '(':
			case ')':
				lex_type = c;
				break;

			case '*':
				lex_type = CLOSURE;
				break;
				
			case '|':
				lex_type = UNION;
				break;

			default:
				lex_value = c;	
				lex_type = CONCAT;
		}
		if( lex_type != NONE )
		{
			return lex_type;
		}
	}

	lex_type = DONE;
	return DONE;
}

static void str_add( char *buf, char c )
{
	for( ; *buf != 0; buf++ )
	{
		if( *buf == c )
		{
			return;
		}
	}
	*buf = c;
	buf++;
	*buf = 0;
}

char *scan_get_regc( const char *reg_expr, char *buf )
{
	for( ; *reg_expr != 0; reg_expr ++ )
	{
		char c = *reg_expr;
		if( c != '(' &&
			c != ')' &&
			c != '*' &&
			c != '|' )	
		{
			str_add( buf, c );
		}
	}
	return buf;
}

