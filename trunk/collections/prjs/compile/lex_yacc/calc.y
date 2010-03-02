%{
#include <stdio.h>
%}
%token NAME NUMBER
%%
statement: NAME '=' expression
		 | expression { printf( "=%d\n", $1 ); }
;
expression: NUMBER '+' NUMBER { $$ = $1 + $3; }
		  | NUMBER '-' NUMBER { $$ = $1 - $3; }
		  | NUMBER { $$ = $1; }
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

