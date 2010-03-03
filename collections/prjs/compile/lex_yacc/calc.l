%{
#include "y.tab.h"
extern int sym_lookup( const char *);
%}
%%
[0-9]+ { yylval.dval = atof( yytext ); return NUMBER; }
[ \t] ; /* ignore */
\n { return '\n'; } 
[a-zA-Z][a-zA-Z0-9]* { yylval.vblno = sym_lookup( yytext ); return NAME; }
'$' { return 0; }
. return yytext[0];
%%

