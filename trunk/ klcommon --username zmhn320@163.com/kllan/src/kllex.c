/**
 * @file kllex.c
 * @brief to process lex for kl source
 * @author Kevin Lynx
 */
#include "kllex.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "klmemcheck.h"


/* reserved words */
const struct Token reserved_words[] = {
	{ TK_IF, "if" }, 
	{ TK_ELSE, "else" }, 
	{ TK_WHILE, "while" }, 
	{ TK_DO, "do" }, 
	{ TK_RETURN, "return" }, 
	{ TK_BREAK, "break" }, 
	{ TK_FUNCTION, "function" }, 
	{ TK_ARRAY, "dim" },
	{ 0, NULL }
};

static int lex_lookup_reserved( const char *str )
{
	int i = 0;
	for( ; reserved_words[i].type != 0; ++ i )
	{
		if( strcmp( str, reserved_words[i].string ) == 0 )
		{
			return reserved_words[i].type;
		}
	}

	return 0;
}

static char lex_next( struct lexState *ls )
{
	if( ls->source[ls->current] == 0 )
	{
		return TK_EOF;
	}
	else
	{
		return ls->source[ls->current++];
	}
}

static void lex_back( struct lexState *ls )
{
	ls->current --;
}
				
static void lex_settoken( struct lexState *ls, int type, const char *str )
{
	ls->token.type = type;
	if( str == 0 )
	{
		ls->token.string = 0;
	}
	else
	{
		ls->token.string = (char*) malloc( strlen( str ) + 1 );
		strcpy( ls->token.string, str );
	}
}

static int lex_read_char( struct lexState *ls )
{
	char c = lex_next( ls );
	char next = lex_next( ls );
	if( c != '\'' && next == '\'' )
	{
		ls->token.type = TK_CHAR;
		ls->token.string = (char*) malloc( sizeof( char ) * 2 );
		sprintf( ls->token.string, "%c", c );
	}
	else if( c == '\\' && ( next == 'n' || next == 't' ) && lex_next( ls ) == '\'' )
	{
		ls->token.type = TK_CHAR;
		ls->token.string = (char*) malloc( sizeof( char ) * 2 );
		switch( next )
		{
			case 'n':
				ls->token.string[0] = (char)10;
				break;

			case 't':
				ls->token.string[0] = (char)9;
				break;
		}
		ls->token.string[1] = 0;
	}
	else
	{
		lex_settoken( ls, TK_ERROR, 0 );
	}

	return ls->token.type;
}	

static int lex_read_string( struct lexState *ls )
{
	char tmp[STRING_MAX_LEN + 1];
	size_t pos = 0;
	char c = lex_next( ls );
	char prev_c = 0;
	while( c != '\n' && c != '"' && pos < STRING_MAX_LEN )
	{
		if( ( c == 'n' || c == 't' ) && prev_c == '\\' )
		{
			switch( c )
			{
				case 'n':
					tmp[pos-1] = (char)10;
					break;

				case 't':
					tmp[pos-1] = (char)9;
					break;
			}
		}
		else
		{
			tmp[pos++] = c;
		}
		prev_c = c;
		c = lex_next( ls );
	}
	if( pos >= STRING_MAX_LEN )
	{
		lex_settoken( ls, TK_ERROR, 0 );
	}
	else
	{
		tmp[pos] = 0;
		lex_settoken( ls, TK_STRING, tmp );
	}

	return ls->token.type;
}

static int lex_read_number( struct lexState *ls, char c )
{
	char tmp[NUMBER_MAX_LEN + 1];
	size_t pos = 0;
	int dot_flag = 0; /* to identify float number */
	do
	{
		tmp[pos++] = c;
		c = lex_next( ls );
		if( !dot_flag && c == '.' )
		{
			dot_flag = 1;
			tmp[pos++] = c;
			c = lex_next( ls );
		}
	}while( isdigit( c ) && pos < NUMBER_MAX_LEN );

	lex_back( ls );
	tmp[pos] = 0;
	lex_settoken( ls, dot_flag ? TK_FLOAT : TK_NUM, tmp );
	
	return ls->token.type;
}

static int lex_read_id( struct lexState *ls, char c )
{
	char tmp[ID_MAX_LEN + 1];
	size_t pos = 0;
	do
	{
		tmp[pos++] = c;
		c = lex_next( ls );
	}while( isdigit( c ) || isalpha( c ) || c == '_' );
   
   	lex_back( ls );
	tmp[pos] = 0;
	lex_settoken( ls, TK_ID, tmp );
	return ls->token.type;
}

void lex_setinput( struct lexState *ls, char *source, lex_errorfn lex_error )
{
	ls->lineno = 1;
	ls->source = source;
	ls->current = 0;
	ls->token.type = TK_ERROR;
	ls->token.string = 0;
	ls->lex_error = lex_error;
}	

int lex_token( struct lexState *ls )
{
	char c = lex_next( ls );
	int done = 0;
	if( c == TK_EOF )
	{
		return TK_EOF;
	}
	for( ; c != TK_EOF && !done; )
	{
		done = 1;
		switch( c )
		{
			case '\n':
			case '\r':
				done = 0;
				ls->lineno ++;
				break;
			
			case ' ':
			case '\t':
				done = 0;
				break;

			case '>':
				{
					if( lex_next( ls ) == '=' )
					{
						lex_settoken( ls, TK_GE, ">=" );
					}
					else
					{
						lex_back( ls );
						lex_settoken( ls, '>', ">" );
					}
				}
				break;

			case '<':
				{
					if( lex_next( ls ) == '=' )
					{
						lex_settoken( ls, TK_LE, "<=" );
					}
					else
					{
						lex_back( ls );
						lex_settoken( ls, '<', "<" );
					}
				}
				break;
			
			case '=':
				{
					if( lex_next( ls ) == '=' )
					{
						lex_settoken( ls, TK_EQ, "==" );
					}
					else
					{
						lex_back( ls );
						lex_settoken( ls, '=', "=" );
					}
				}
				break;

			case '!':
				{
					if( lex_next( ls ) == '=' )
					{
						lex_settoken( ls, TK_NE, "!=" );
					}
					else
					{
						lex_back( ls );
						lex_settoken( ls, '!', "!" );
					}
				}
				break;

			case '|':
				{
					if( lex_next( ls ) == '|' )
					{
						lex_settoken( ls, TK_OR, "||" );
					}
					else
					{
						lex_settoken( ls, TK_ERROR, 0 );
						ls->lex_error( ls->lineno, ">>lex error->expect '|'" );
					}
				}
				break;

			case '&':
				{
					if( lex_next( ls ) == '&' )
					{
						lex_settoken( ls, TK_AND, "&&" );
					}
					else
					{
						lex_settoken( ls, TK_ERROR, 0 );
						ls->lex_error( ls->lineno, ">>lex error->expect '&'" );
					}
				}
				break;

#define SINGLE_TOKEN( t, str ) \
			case t: \
			{ \
				lex_settoken( ls, t, str ); \
			} \
			break

			SINGLE_TOKEN( '[', "[" );
			SINGLE_TOKEN( ']', "]" );
			SINGLE_TOKEN( '(', "(" );
			SINGLE_TOKEN( ')', ")" );
			SINGLE_TOKEN( '{', "{" );
			SINGLE_TOKEN( '}', "}" );
			SINGLE_TOKEN( '+', "+" );
			SINGLE_TOKEN( '-', "-" );
			SINGLE_TOKEN( '*', "*" );
			SINGLE_TOKEN( '/', "/" );
			SINGLE_TOKEN( '%', "%" );
			SINGLE_TOKEN( ',', "," );
			SINGLE_TOKEN( ';', ";" );

			case '\'':
				{
					int t = lex_read_char( ls );
					if( t == TK_ERROR )
					{
						ls->lex_error( ls->lineno, ">>lex error->read char constant error" );
					}
				}
				break;

			case '"':
				{
					int t = lex_read_string( ls );
					if( t == TK_ERROR )
					{
						ls->lex_error( ls->lineno, ">>lex error->read string constant error" );
					}
				}
				break;

			default:
				{
					if( isdigit( c ) )
					{
						lex_read_number( ls, c );
					}
					else if( isalpha( c ) || c == '_' )
					{
						int t;
						lex_read_id( ls, c );
						/* check whether it's a reserved word */
						if( ( t = lex_lookup_reserved( ls->token.string ) ) != 0 )
						{
							ls->token.type = t;
						}
					}
					else 
					{
						lex_settoken( ls, TK_ERROR, 0 );
						ls->lex_error( ls->lineno, ">>lex error->unknown token character" );
					}
				}	
		}	
		if( !done )
		{
			c = lex_next( ls );
		}
	}	

	if( c == TK_EOF )
	{
		lex_settoken( ls, TK_EOF, 0 );
	}
	return ls->token.type;	
}	



