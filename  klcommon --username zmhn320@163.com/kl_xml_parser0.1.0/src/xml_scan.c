/**
  @file xml_scan.c
  @author Kevin Lynx
  @see also Tiny Compiler scan.c
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
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

#ifdef XML_USE_FILE
static char line_buf[BUFLEN];
static FILE *filein = 0;
#else
static const char *content_buf = 0;
#endif
static unsigned long lineno = 0;
static int line_pos = 0;
static int buf_size = 0;

#ifdef XML_USE_FILE
static int getNextChar()
{
	if( !( line_pos < buf_size ) )
	{
		/* get the next line */
		lineno ++;
		if( fgets( line_buf, BUFLEN - 1, filein ) )
		{
			buf_size = (int)strlen( line_buf );
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
#else
static int getNextChar()
{
	if( line_pos >= buf_size )
	{
		return EOF;
	}
	return content_buf[line_pos++];
}
#endif

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

#ifdef XML_USE_FILE
void xmlscan_init( FILE *fp )
{
	lineno = 0;
	filein = fp;
	line_pos = 0;
	buf_size = 0;
	line_buf[0] = 0;
}
#else
void xmlscan_init( const void *buf, size_t size )
{
	content_buf = buf;
	lineno = 0;
	line_pos = 0;
	buf_size = (int)size;
}
#endif


