/*
   tinyc.y
   yacc file for tinyc practice
   Kevin Lynx
   4.2.2010
*/
%{
#include <stdio.h>
#include <stdlib.h>
#include "symtab.h"
#include "code.h"

extern int yylex();
extern FILE *yyin;
extern char *yytext;

FILE *code;
int TraceCode = 0;

/* memory location */
static int memloc = 0;
/* temp memory pointer */
static int tmpOffset = 0;

%}
%union {
	int loc;
}
%token OR_OP AND_OP EQ_OP NE_OP LE_OP GE_OP
%token IDENTIFIER 
%token NUM
%token IF ELSE WHILE BREAK
%token READ WRITE
%type <loc> identifier
%nonassoc IFX
%nonassoc ELSE
%%
statement_list
	: statement
	| statement_list statement
	;

statement
	: expression_statement
	| compound_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	| io_statement
	;

expression_statement
	: ';'
	| assignment_expr ';'
	;

assignment_expr
	: identifier '=' expr { 
		emitRM( "ST", ac, $1, gp, "store value" );	
	}
	;

expr
	: term {
		/* term will push left operand itself, ignore here */
	}
	| expr '+' term  {
		/* load right operand to ac1 */
		emitRM( "LD", ac1, ++tmpOffset, mp, "load left" );
		/* load left operand to ac */
		emitRM( "LD", ac, ++tmpOffset, mp, "load right" );
		emitRO( "ADD", ac, ac, ac1, "op +" );
		/* push the result */
		emitRM( "ST", ac, tmpOffset--, mp, "push add result" );
	}
	| expr '-' term {
		/* load right operand to ac1 */
		emitRM( "LD", ac1, ++tmpOffset, mp, "load left" );
		/* load left operand to ac */
		emitRM( "LD", ac, ++tmpOffset, mp, "load right" );
		emitRO( "SUB", ac, ac, ac1, "op -" );
		/* push the result */
		emitRM( "ST", ac, tmpOffset--, mp, "push sub result" );
	}
	;

term
	: factor {
		/* push left operand */
		emitRM( "ST", ac, tmpOffset--, mp, "push left" );	
	}
	| term '*' factor {
		/* load left operand to ac1 */
		emitRM( "LD", ac1, ++tmpOffset, mp, "load left" );
		/* and the right operand is in ac */
		emitRO( "MUL", ac, ac1, ac, "op *" );
		/* push the result */
		emitRM( "ST", ac, tmpOffset--, mp, "push mul result" );
	}
	| term '/' factor {
		/* load left operand to ac1 */
		emitRM( "LD", ac1, ++tmpOffset, mp, "load left" );
		/* and the right operand is in ac */
		emitRO( "DIV", ac, ac1, ac, "op /" );
		/* push the result */
		emitRM( "ST", ac, tmpOffset--, mp, "push div result" );
	}
	;

factor
	: primary_expr { /* do nothing */ }
	| '+' primary_expr { /* do nothing */ }
	| '-' primary_expr { 
		emitRM( "LDC", ac1, 0, 0, "load zero" );
		emitRO( "SUB", ac, ac1, ac, "minus" );
	}
	;

primary_expr
	: identifier {
		emitRM( "LD", ac, $1, gp, "load id value" );
	}
	| NUM {
		emitRM( "LDC", ac, atof( yytext ), 0, "load const" );	
	}
	| '(' expr ')'
	;

identifier
	: IDENTIFIER {
		int loc = sym_lookup( yytext );
		if( loc < 0 )
		{
			loc = memloc;
			sym_insert( yytext, memloc++ );
		}
		$$ = loc;
	}
	;

compound_statement
	: '{' '}'
	| '{' statement_list '}'
	;

logical_or_expr
	: logical_and_expr
	| logical_or_expr OR_OP logical_and_expr
	;

logical_and_expr
	: equality_expr
	| logical_and_expr AND_OP equality_expr
	;

equality_expr
	: relational_expr
	| equality_expr EQ_OP relational_expr
	| equality_expr NE_OP relational_expr
	;

relational_expr
	: relational_factor
	| relational_expr '>' relational_factor
	| relational_expr '<' relational_factor
	| relational_expr LE_OP relational_factor
	| relational_expr GE_OP relational_factor
	;

relational_factor
	: relational_primary
	| '!' relational_primary
	| '+' relational_primary
	| '-' relational_primary
	;

relational_primary
	: identifier 
	| NUM
	;

selection_statement
	: IF '(' logical_or_expr ')' statement %prec IFX
	| IF '(' logical_or_expr ')' statement ELSE statement
	;

iteration_statement
	: WHILE '(' logical_or_expr ')' statement
	;

jump_statement
	: BREAK ';'
	;

io_statement
	: READ expr ';' {
		emitRO( "IN", ac, 0, 0, "input to ac" );
	}
	| WRITE expr ';' {
		/* load the expr result from mp */
		emitRM( "LD", ac, ++tmpOffset, mp, "load expr" );
		emitRO( "OUT", ac, 0, 0, "write ac" );
	}
	;
%%
void yyerror( const char *s )
{
	fprintf( stderr, "%s\n", s );
}

void prelude()
{
	emitRM( "LD", mp, 0, ac, "" );
	emitRM( "ST", ac, 0, ac, "" );
}

int main( int argc, char **argv )
{
	if( argc < 2 )
	{
		fprintf( stderr, "Usage:%s <filename>\n", argv[0] );
		exit( -1 );
	}
	yyin = fopen( argv[1], "r" );
	if( yyin == 0 )
	{
		fprintf( stderr, "Open %s failed\n", argv[1] );
		exit( -1 );
	}
	{
		char output[256] = { 0 };
		int l = strcspn( argv[1], "." );
		strncpy( output, argv[1], l );
		strcat( output, ".tm" );
		code = fopen( output, "w" );
		if( code == 0 )
		{
			fprintf( stderr, "Unable to open %s\n", output );
			exit( -1 );
		}
	}
	if( argc > 2 )
	{
		TraceCode = atoi( argv[2] );
	}
	prelude();
	yyparse();
	sym_dump();
	sym_clear();	
	fclose( yyin );
	fclose( code );
	return 0;
}

