/*
   completed by Kevin Lynx 5.26.2010
 */
%{
#include <string.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "y.tab.h"
#include "errormsg.h"

int charPos=1;

int yywrap(void)
{
 charPos=1;
 return 1;
}


void adjust(void)
{
 EM_tokPos=charPos;
 charPos+=yyleng;
}

void comment();

%}
l [a-zA-Z_]
d [0-9]
%%
[ \t] {adjust(); continue;}
[\n\r]|\r\n {adjust(); EM_newline(); continue;}
","	 {adjust(); return COMMA;}
":"	 {adjust(); return COLON; }
";"	 {adjust(); return SEMICOLON; }
"("	 {adjust(); return LPAREN; }
")"	 {adjust(); return RPAREN; }
"["	 {adjust(); return LBRACK; }
"]"	 {adjust(); return RBRACK; }
"{"	 {adjust(); return LBRACE; }
"}"	 {adjust(); return RBRACE; }
"."	 {adjust(); return DOT; }
"+"	 {adjust(); return PLUS; }
"-"	 {adjust(); return MINUS; }
"*"	 {adjust(); return TIMES; }
"/"	 {adjust(); return DIVIDE; }
"="	 {adjust(); return EQ; }
"<"	 {adjust(); return LT; }
">"	 {adjust(); return GT; }
"&"	 {adjust(); return AND; }
"|"	 {adjust(); return OR; }
"<>" {adjust(); return NEQ; }
"<=" {adjust(); return LE; }
">=" {adjust(); return GE; }
":=" {adjust(); return ASSIGN; }
for  	 {adjust(); return FOR;}
while	{adjust(); return WHILE;}
to	{adjust(); return TO;}
break	{adjust(); return BREAK; }
let	{adjust(); return LET; }
in {adjust(); return IN; }
end {adjust(); return END; }
function {adjust(); return FUNCTION; }
var {adjust(); return VAR; }
type {adjust(); return TYPE; }
array {adjust(); return ARRAY; }
if {adjust(); return IF; }
then {adjust(); return THEN; }
else {adjust(); return ELSE; }
do {adjust(); return DO; }
of {adjust(); return OF; }
nil {adjust(); return NIL; }
"/*" { adjust(); comment(); continue; }
{d}+	 {adjust(); yylval.ival = atoi( yytext ); return INT; }
\"(\\.|[^\\"])*\" {adjust(); yylval.sval = String( yytext ); return STRING; }
{l}+{d}* {adjust(); yylval.sval=String(yytext); return ID; }
.	 {adjust(); EM_error(EM_tokPos,"illegal token");}

%%
void comment()
{
	char c, c1;
loop:
	while ((c = input()) != '*' && c != 0)
	{
		if( c == '\n' )
		{
			EM_newline();
		}
		else
		{
			EM_tokPos=charPos;
			charPos+=1;
		}
	}
	if ((c1 = input()) != '/' && c != 0)
	{
		EM_tokPos-=1;
		charPos -=1;
		unput(c1);
		goto loop;
	}
	EM_tokPos=charPos;
	charPos+=1;
}

