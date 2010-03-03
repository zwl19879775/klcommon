%{
#include <stdio.h>
double vbltable[26];
%}
%token <vblno> NAME
%token <dval> NUMBER
%type <dval> expression
%union {
	double dval;
	int vblno;
}
%%
statement: NAME '=' expression { vbltable[$1] = $3; }
		 | expression { printf( "=%g\n", $1 ); }
;
expression: expression '+' NUMBER { $$ = $1 + $3; }
		  | expression '-' NUMBER { $$ = $1 - $3; }
		  | NUMBER { $$ = $1; }
		  | NAME { $$ = vbltable[$1]; }
;
%%
void yyerror( const char *s )
{
	fprintf( stderr, "%s\n", s );
}

int main()
{
	yyparse();
	return 0;
}

