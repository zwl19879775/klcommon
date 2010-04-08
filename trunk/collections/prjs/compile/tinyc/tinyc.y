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
		emitRM( "LD", ac1, ++tmpOffset, mp, "load right" );
		/* load left operand to ac */
		emitRM( "LD", ac, ++tmpOffset, mp, "load left" );
		emitRO( "ADD", ac, ac, ac1, "op +" );
		/* push the result */
		emitRM( "ST", ac, tmpOffset--, mp, "push add result" );
	}
	| expr '-' term {
		/* load right operand to ac1 */
		emitRM( "LD", ac1, ++tmpOffset, mp, "load right" );
		/* load left operand to ac */
		emitRM( "LD", ac, ++tmpOffset, mp, "load left" );
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
	: logical_and_expr {
		/* load the result into ac */
		emitRM( "LD", ac, ++tmpOffset, mp, "load result" );
	}
	| logical_or_expr OR_OP logical_and_expr {
		/* load right operand to ac1 */
		emitRM( "LD", ac1, ++tmpOffset, mp, "load right" );
		/* load left operand to ac */
		emitRM( "LD", ac, ++tmpOffset, mp, "load left" );
		/* jump if true */
		emitRM( "JNE", ac, 2, pc, "jmp if true" );
		emitRM( "JNE", ac1, 1, pc, "jmp if true" );
		/* false case, ac and ac1 are all store '0' */
		/* unconditinal jmp */
		emitRM( "LDA", pc, 1, pc, "unconditional jmp" );
		/* true case */
		emitRM( "LDC", ac, 1, 0, "load const 0" );
		/* the result is in ac */
	}
	;

logical_and_expr
	: logical_not_expr
	| logical_and_expr AND_OP logical_not_expr {
		/* load right operand to ac1 */
		emitRM( "LD", ac1, ++tmpOffset, mp, "load right" );
		/* load left operand to ac */
		emitRM( "LD", ac, ++tmpOffset, mp, "load left" );
		/* jump if false */
		emitRM( "JEQ", ac, 2, pc, "jmp if false" );
		emitRM( "JEQ", ac1, 1, pc, "jmp if false" );
		/* true case, ac and ac1 are all store '1' */
		/* unconditinal jmp */
		emitRM( "LDA", pc, 1, pc, "unconditional jmp" );
		/* false case */
		emitRM( "LDC", ac, 0, 0, "load const 0" );
		/* push result */
		emitRM( "ST", ac, tmpOffset--, mp, "push result" );
	}
	;

logical_not_expr
	: relational_expr { /* the result is in mp */ }
	| '!' relational_expr {
		/* load the operand */
		emitRM( "LD", ac, ++tmpOffset, mp, "load '!' operand" );
		/* because true is '1', false is '0' */
		emitRM( "LDC", ac1, 1, 0, "load const 1" );
		emitRM( "SUB", ac, ac1, ac, "op !" );
		/* push the result */
		emitRM( "ST", ac, tmpOffset--, mp, "push result" );
	}
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
	: IF '(' logical_or_expr IfBracket statement %prec IFX {
		int curLoc = emitSkip( 0 );
		emitBackup( pop_saved_loc().loc );	
		emitRM_Abs( "JEQ", ac, curLoc, "if:jmp to if end" );
		emitRestore();
	}
	| IF '(' logical_or_expr IfBracket statement ElseToken statement {
		int curLoc = emitSkip( 0 );
		emitBackup( pop_saved_loc().loc );
		emitRM_Abs( "LDA", pc, curLoc, "jmp to if end" );
		emitRestore();
	}
	;

IfBracket
	: ')' {
		/* save loc to backpatch later */
		int loc = emitSkip( 1 );
		push_saved_loc( loc, 0 );	
	}
	;

ElseToken
	: ELSE {
		int loc = emitSkip( 1 );
		int curLoc = emitSkip( 0 );
		emitBackup( pop_saved_loc().loc );
		emitRM_Abs( "JEQ", ac, curLoc, "if:jmp to else" );
		emitRestore();
		push_saved_loc( loc, 0 );
	}
	;

iteration_statement
	: WhileToken '(' logical_or_expr WhileBracket statement {
		int curLoc = emitSkip( 0 );
		/* backpatch break statement */
		break_code( curLoc + 1 );
		/* backpatch the test logical expr */
		emitBackup( pop_saved_loc().loc );
		emitRM_Abs( "JEQ", ac, curLoc + 1, "while:jmp to end" );
		emitRestore();
		/* unconditional jmp to while begin */
		emitRM_Abs( "LDA", pc, pop_saved_loc().loc, "while:jmp to begin" );	
	}
	;

WhileBracket
	: ')' {
		/* save loc to backpatch later */
		int loc = emitSkip( 1 );
		push_saved_loc( loc, WhileStart );
	}
	;

WhileToken
	: WHILE {
		/* while...end whill jmp here */
		int loc = emitSkip( 0 );
		push_saved_loc( loc, 0 );
	}
	;

jump_statement
	: BREAK ';' {
		int loc = emitSkip( 1 );
		push_saved_loc( loc, 0 );
	}
	;

io_statement
	: READ  identifier ';' {
		emitRO( "IN", ac, 0, 0, "input to ac" );
		emitRM( "ST", ac, $2, gp, "store value" );
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

void prelude_code()
{
	emitRM( "LD", mp, 0, ac, "" );
	emitRM( "ST", ac, 0, ac, "" );
}

void break_code( int eloc )
{
	while( saved_loc_top.flag != WhileStart )
	{
		emitBackup( pop_saved_loc().loc );
		emitRM_Abs( "LDA", pc, eloc, "while:break to end" );
		emitRestore();	
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
	/* load right operand to ac1 */
	emitRM( "LD", ac1, ++tmpOffset, mp, "load right" );
	/* load left operand to ac */
	emitRM( "LD", ac, ++tmpOffset, mp, "load left" );
	emitRO( "SUB", ac, ac, ac1, "op -" );
	emitRM( op, ac, 2, pc, "br if true" );
	emitRM( "LDC", ac, 0, 0, "false case" );
	emitRM( "LDA", pc, 1, pc, "unconditional jmp" );
	emitRM( "LDC", ac, 1, 0, "true case" );
	/* push the result */
	emitRM( "ST", ac, tmpOffset--, mp, "push result" );
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
	prelude_code();
	yyparse();
	sym_dump();
	sym_clear();	
	fclose( yyin );
	fclose( code );
	return 0;
}

