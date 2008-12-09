/**
  @file xml_scan.c
  @author Kevin Lynx
  @see also Tiny Compiler scan.c
*/
#include <stdio.h>
#include "xml_scan.h"

/**
  dfa state
*/
enum 
{
	START,
	IN_EE, /* in '/>' */
	IN_EBE, /* in '</' */
	IN_ID,
	IN_STRING,
	DONE
};

#define BUFLEN 512

static char line_buf[BUFLEN];
static int line_pos = 0;
static int buf_size = 0;
static FILE *filein = 0;
static unsigned long lineno = 0;

static int getNextChar()
{
	if( !( line_pos < buf_size ) )
	{
		/* get the next line */
		lineno ++;
		if( fgets( line_buf, BUFLEN - 1, filein ) )
		{
			buf_size = strlen( line_buf );
			line_pos = 0;
			return line_buf[line_pos++];
		}
		else
		{
			return EOF;
		}
	}
	else
	{
		return line_buf[line_pos++];
	}
}

static void ungetNextChar()
{
	line_pos --;
}

struct Token xmlscan_gettoken()
{
	struct Token token;
	int tokenStringIndex = 0;
	int state = START;
	int save;
	while( state != DONE )
	{
		int c = getNextChar();
		save = 1;

		switch( state )
		{
			case START:
				{
					if( isalpha( c ) )
					{
						state = IN_ID;
					}
					else if( c == '"' )
					{
						state = IN_STRING;
					}
					else if( c == '/' )
					{
						state = IN_EE;
					}
					else if( c == '<' )
					{
						int next = getNextChar();
						if( next != '/' )
						{
							state = DONE;
							token.type = LB;
						}
						else
						{
							state = IN_EBE;
						}
						ungetNextChar();
					}
					else if( c == ' ' || c == '\t' || c == '\n' )
					{
						save = 0;
					}
					else 
					{
						/* token with a single character */
						state = DONE;
						switch( c )
						{
							case EOF:
								save = 0;
								token.type = ENDFILE;
								break;

							case '=':
								token.type = ASSIGN;
								break;

							case '>':
								token.type = RB;
								break;
						}
					}
				}
				break;

			case IN_ID:
				{
					if( !isalpha( c ) )
					{
						ungetNextChar();
						save = 0;
						state = DONE;
						token.type = ID;
					}
				}
				break;

			case IN_STRING:
				{
					if( c == '"' )
					{
						state = DONE;
						token.type = VALUE;
					}
				}
				break;

			case IN_EE:
				{
					state = DONE;
					if( c == '>' )
					{
						token.type = EE;
					}
					else
					{
						/* error */
						ungetNextChar();
						token.type = ERROR;
						save = 0;	
					}
				}
				break;

			case IN_EBE:
				{
					state = DONE;
					if( c == '/' )
					{
						token.type = EBE;
					}
					else
					{
						/* error */
						ungetNextChar();
						token.type = ERROR;
						save = 0;
					}
				}
				break;

			default:
				/* error */
				break;
		}
		if( save && tokenStringIndex < TOKEN_LENGTH )
		{
			token.string[tokenStringIndex++] = (char) c;
		}
		if( state == DONE )
		{
			token.string[tokenStringIndex] = 0;
		}
	}

	if( token.type == VALUE )
	{
		/* to remove '' */
		char text[TOKEN_LENGTH];
		strcpy( text, token.string );
		strncpy( token.string, &text[1], TOKEN_LENGTH );
		token.string[strlen( token.string ) - 1] = 0;	
	}

	return token;
}

unsigned long xmlscan_line()
{
	return lineno;
}

void xmlscan_init( FILE *fp )
{
	lineno = 0;
	filein = fp;
	line_pos = 0;
	buf_size = 0;
	line_buf[0] = 0;
}



