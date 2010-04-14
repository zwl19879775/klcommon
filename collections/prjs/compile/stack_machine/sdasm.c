/**
 * SM deAssember
 * Kevin Lynx
 * 4.14.2010
 */
#include "sm.h"
#include <stdio.h>
#include <string.h>

extern int get_operand_count( int op );

/* map to op_type */
const char *op_desc[] = {
	"HALT", "IN", "OUT", "ADD", "SUB", "MUL", "DIV",
	"DUP",
	"LD", "ST", "LDC", "JLT", "JLE", "JGT", "JGE", "JEQ", "JNE", "JMP", 0
};


void dasm_output( const char *file, const Instruction *insts, int size )
{
	FILE *fp;
	char f[256];
	int i;
	strcpy( f, file );
	strcat( f, ".dasm" );
	fp = fopen( f, "w" );
	for( i = 0; i < size && insts[i].op != opHalt; ++ i )
	{
		fprintf( fp, "%3d\t%s", i, op_desc[insts[i].op] );
		if( get_operand_count( insts[i].op ) > 0 )
		{
			fprintf( fp, "\t%d", insts[i].arg );
		}
		fputc( '\n', fp );
	}
	fclose( fp );
}

