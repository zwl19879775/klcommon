%{
#include <stdio.h>
#include <math.h>

#define SYM_SIZE (64)

struct symtab
{
	char *name;
	double (*func)( double );
	double value;
} symtab[SYM_SIZE];

%}
%token <vblno> NAME
%token <dval> NUMBER
%type <dval> expression
%type <dval> term
%type <dval> factor
%union {
	double dval;
	int vblno;
}
%%
statement_list: statement '\n'
			  | statement_list statement '\n'
;
statement: NAME '=' expression { 
		 				if( $1 == -1 ) yyerror( "symtable error" );
						else symtab[$1].value = $3;
					}
		 | expression { printf( "=%g\n", $1 ); }
;
expression: expression '+' term { $$ = $1 + $3; }
		  | expression '-' term { $$ = $1 - $3; }
		  | term 
;
term: term '*' factor { $$ = $1 * $3; }
	| term '/' factor { 
						if( $3 == 0 ) yyerror( "divied by 0" );
						else $$ = $1 / $3; 
					  }
    | factor
;
factor: NUMBER { $$ = $1; }
	| NAME { $$ = symtab[$1].value; }
	| '(' expression ')' { $$ = $2; }
	| NAME '(' expression ')' { $$ = symtab[$1].func( $3 ); }
;
%%
void yyerror( const char *s )
{
	fprintf( stderr, "yyerror: %s\n", s );
}

int sym_lookup( const char *name )
{
	int i;
	for( i = 0; i < SYM_SIZE; ++ i )
	{
		if( symtab[i].name == 0 )
		{
			symtab[i].name = strdup( name );
			return i;
		}
		if( strcmp( name, symtab[i].name ) == 0 )
		{
			return i;
		}
	}
	return -1;
}

void add_func( const char *name, double (*f)( double ) )
{
	int i = sym_lookup( name );
	symtab[i].func = f;
}

void sym_free()
{
	int i;
	for( i = 0; i < SYM_SIZE; ++ i )
	{
		if( symtab[i].name == 0 )
			break;
		free( symtab[i].name );
	}
}

int main()
{
	add_func( "sqrt", sqrt );
	yyparse();
	sym_free();
	return 0;
}

