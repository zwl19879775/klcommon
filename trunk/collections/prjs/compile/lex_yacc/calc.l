%{
#include "y.tab.h"
extern double vbltable[26];
%}
%%
[0-9]+ { yylval.dval = atoi( yytext ); return NUMBER; }
[ \t] ; /* ignore */
\n return 0;
[a-z] { yylval.vblno = yytext[0] - 'a'; return NAME; }
. return yytext[0];
%%

