/*
  To generate the scanner for xml files.
*/
%{
#include <stdio.h>
#include "xml_scan.h"

static unsigned long lineno;

%}

LeftBracket <
RightBracket >
ElementEnd \/>
ElementBlockEnd <\/
Letter [a-zA-Z]
Digit [0-9]
Num {Digit}+
ID {Letter}+({Letter}|{Digit})*
WhiteSpace [ \t]+
Assign =
String \"[^\"]*\"
NewLine \n

%%

{LeftBracket} { return LB; }
{RightBracket} { return RB; }
{ElementEnd} { return EE; }
{ID} { return ID; }
{Assign} { return ASSIGN; }
{String} { return VALUE; }
{ElementBlockEnd} { return EBE; }
{WhiteSpace} { }
{NewLine} { ++lineno; }
. { return ERROR; }
%%

struct Token xmlscan_gettoken()
{
	struct Token token ;
	token.type = yylex();
	if( token.type == VALUE )
	{
		/* to remove '' */
		strncpy( token.string, &yytext[1], TOKEN_LENGTH );
		token.string[strlen( token.string ) - 1] = 0;	
	}
	else if( token.type != ENDFILE )
	{
		strncpy( token.string, yytext, TOKEN_LENGTH );
	}

	return token;
}

int yywrap()
{
	return 1;
}

unsigned long xmlscan_line()
{
	return lineno;
}

void xmlscan_init( FILE *fp )
{
	yyin = fp;
	lineno = 0;
}
