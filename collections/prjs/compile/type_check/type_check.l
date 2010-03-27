%{
/* kevin lynx 3.27.2010 */
#include "y.tab.h"
%}
L	[a-zA-Z_]
D	[0-9]
%%
"char"	{ return CHAR; }
"short"	{ return SHORT; }
"int"	{ return INT; }
"float"	{ return FLOAT; }
"double"	{ return DOUBLE; }
"struct"	{ return STRUCT; }
"{"	{ return '{'; }
"}"	{ return '}'; }
";"	{ return ';'; }
"," { return ','; }

{L}({L}|{D})*	{  return IDENTIFIER; }	
[ \t] { /* ignore */ }
\n { /* ignore */ }
"$" { /* to stop */ return 0; }
. {  /* ignore other characters */ }
%%

