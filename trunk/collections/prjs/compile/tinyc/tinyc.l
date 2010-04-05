/*
   tinyc.l
   lex file for tinyc practice
   Kevin Lynx
   4.2.2010
*/
%{
#include "y.tab.h"
%}
L	[a-zA-Z_]
D	[0-9]
%%
"if"	{ return IF; }
"else"	{ return ELSE; }
"while"	{ return WHILE; }
"break"	{ return BREAK; }
"read" { return READ;/* specially for TM */ } 
"write" { return WRITE; /* specially for TM */ }
{L}({L}|{D})*	{ return IDENTIFIER; }
{D}*	{ return NUM; }
"{"	{ return '{'; }
"}"	{ return '}'; }
"="	{ return '='; }
"("	{ return '('; }
")"	{ return ')'; }
"&&"	{ return AND_OP; }
"||"	{ return OR_OP; }
"!"	{ return '!'; }
"=="	{ return EQ_OP; }
"!="	{ return NE_OP; }
">="	{ return GE_OP; }
"<="	{ return LE_OP; }
">"	{ return '>'; }
"<"	{ return '<'; }
";"	{ return ';'; }
"+"	{ return '+'; }
"-"	{ return '-'; }
"*"	{ return '*'; }
"/"	{ return '/'; }
"\n" { /* ignore */ }
"$"	{ /* stop */ return 0; }
.	{ /* ignore */ }
%%

