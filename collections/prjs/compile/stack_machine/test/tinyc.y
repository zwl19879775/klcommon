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
#include "../sm.h"
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
	: identifier {
		emitCodeArg( opLdc, $1 );
	} '=' expr {
		emitCode( opSt );
	}
	;

expr
	: term 
	| expr '+' term {
		emitCode( opAdd );
	}
	| expr '-' term {
		emitCode( opSub );
	}
	;

term
	: factor 
	| term '*' factor {
		emitCode( opMul );
	}
	| term '/' factor {
		emitCode( opDiv );
	}
	;

factor
	: primary_expr
	| '+' primary_expr 
	| '-' primary_expr {
		emitCodeArg( opLdc, 0 );
		emitCode( opSub );
	}
	;

primary_expr
	: identifier {
		emitCodeArg( opLdc, $1 );
		emitCode( opLd );	
	}
	| NUM {
		emitCodeArg( opLdc, atof( yytext ) );
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

relational_expr
	: expr 
	| relational_expr '>' expr {
		relational_code( '>' );
	}
	| relational_expr '<' expr {
		relational_code( '<' );
	}
	| relational_expr LE_OP expr {
		relational_code( LE_OP );
	}
	| relational_expr GE_OP expr {
		relational_code( GE_OP );
	}
	| relational_expr EQ_OP expr {
		relational_code( EQ_OP );
	}
	| relational_expr NE_OP expr {
		relational_code( NE_OP );
	}
	;

selection_statement
	: IF '(' relational_expr ')' IFACTION statement %prec IFX {
		int curLoc = emitSkip( 0 );
		emitBackup( pop_saved_loc().loc );
		emitCodeArg( opJeq, curLoc );
		emitRestore();
	}
	| IF '(' relational_expr ')' IFACTION statement ELSE {
		int loc = emitSkip( 1 );
		int curLoc = emitSkip( 0 );
		emitBackup( pop_saved_loc().loc );
		emitCodeArg( opJeq, curLoc );
		emitRestore();
		push_saved_loc( loc, 0 );
	} statement {
		int curLoc = emitSkip( 0 );
		emitBackup( pop_saved_loc().loc );
		emitCodeArg( opJmp, curLoc );
		emitRestore();	
	}
	;

IFACTION
	: { 
		int loc = emitSkip( 1 );
		push_saved_loc( loc, 0 );
	}
	;

iteration_statement
	: WHILE {
		int loc = emitSkip( 0 );
		push_saved_loc( loc, 0 );	
	} '(' relational_expr ')' WHILEACTION statement {
		int curLoc = emitSkip( 0 ); /* + jmp to while begin */
		break_code( curLoc + 1 ); /* backpatch break statement */
		emitBackup( pop_saved_loc().loc ); /* backpatch while expr */
		emitCodeArg( opJeq, curLoc + 1 ); /* jmp to while end */
		emitRestore();
		emitCodeArg( opJmp, pop_saved_loc().loc ); /* jmp to while begin */
	}
	;

WHILEACTION
	: {
		int loc = emitSkip( 1 );
		push_saved_loc( loc, WhileStart );
	}
	;

jump_statement
	: BREAK ';' {
		int loc = emitSkip( 0 );
		push_saved_loc( loc, 0 );
	}
	;

io_statement
	: READ  identifier ';' {
		emitCodeArg( opLdc, $2 );
		emitCode( opIn );
		emitCode( opSt );
	}
	| WRITE expr ';' {
		emitCode( opOut );		
	}
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
		emitBackup( pop_saved_loc().loc );
		emitCodeArg( opJmp, eloc );
		emitRestore();
	}
}

void relational_code( int type )
{
	int op;
	switch( type )
	{
	case '<':
		op = opJlt;
		break;
	case '>':
		op = opJgt;
		break;
	case EQ_OP:
		op = opJeq;
		break;
	case NE_OP:
		op = opJne;
		break;
	case LE_OP:
		op = opJle;
		break;
	case GE_OP:
		op = opJge;
	default:
		op = opInvalid;
	}
	if( op == opInvalid )
	{
		return;
	}	
	emitCode( opSub );
	emitCodeRel( op, 2 );
	emitCodeArg( opLdc, 0 ); /* false */
	emitCodeRel( opJmp, 1 );
	emitCodeArg( opLdc, 1 ); /* true */
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
		strcat( output, ".sm" );
	}
	yyparse();
	emitDone( output );
	sym_dump();
	sym_clear();	
	fclose( yyin );
	return 0;
}

