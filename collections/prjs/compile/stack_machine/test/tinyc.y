/*
   tinyc.y
   yacc file for tinyc practice
   Kevin Lynx
   4.2.2010
   4.12.2010 modified for SM a simple virtual machine written by myself
*/
%{
#include <stdio.h>
#include <stdlib.h>
#include "symtab.h"
#include "code.h"

extern int yylex();
extern FILE *yyin;
extern char *yytext;

/* memory location */
static int memloc = 0;
/* saved code locatation for backpatch */
#define SAVED_LOC_SIZE (128)
typedef struct locTag
{
	int loc;
	int flag;
} locTag;
enum { WhileStart = 1 };
static locTag savedLoc[SAVED_LOC_SIZE];
static int saved_pos = 0;
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
	: identifier '=' expr
	;

expr
	: term 
	| expr '+' term 
	| expr '-' term 
	;

term
	: factor 
	| term '*' factor
	| term '/' factor
	;

factor
	: primary_expr
	| '+' primary_expr 
	| '-' primary_expr
	;

primary_expr
	: identifier 
	| NUM 
	| '(' expr ')'
	;

identifier
	: IDENTIFIER { $$ = 1; }
	;

compound_statement
	: '{' '}'
	| '{' statement_list '}'
	;

relational_expr
	: expr 
	| relational_expr '>' expr
	| relational_expr '<' expr 
	| relational_expr LE_OP expr 
	| relational_expr GE_OP expr 
	| relational_expr EQ_OP expr 
	| relational_expr NE_OP expr 
	;

selection_statement
	: IF '(' relational_expr ')' statement %prec IFX 
	| IF '(' relational_expr ')' statement ELSE statement
	;

iteration_statement
	: WHILE '(' relational_expr ')' statement
	;

jump_statement
	: BREAK ';'
	;

io_statement
	: READ  identifier ';'
	| WRITE expr ';'
	;
%%
void yyerror( const char *s )
{
	fprintf( stderr, "%s\n", s );
}

void push_saved_loc( int loc, int flag )
{
	locTag l = { loc, flag };
	savedLoc[saved_pos++] = l;
}

locTag pop_saved_loc()
{
	return savedLoc[--saved_pos];
}

#define saved_loc_top savedLoc[saved_pos-1]

void break_code( int eloc )
{
	while( saved_loc_top.flag != WhileStart )
	{
	}
}

void relational_code( int type )
{
	char *op;
	switch( type )
	{
	case '<':
		op = "JLT";
		break;
	case '>':
		op = "JGT";
		break;
	case EQ_OP:
		op = "JEQ";
		break;
	case NE_OP:
		op = "JNE";
		break;
	case LE_OP:
		op = "JLE";
		break;
	case GE_OP:
		op = "JGE";
	default:
		op = 0;
	}
	if( op == 0 )
	{
		return;
	}	
}

int main( int argc, char **argv )
{
	char output[256] = { 0 };
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
		int l = strcspn( argv[1], "." );
		strncpy( output, argv[1], l );
		strcat( output, ".tm" );
	}
	yyparse();
	sym_dump();
	sym_clear();	
	fclose( yyin );
	return 0;
}

