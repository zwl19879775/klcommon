/**
* SM assembler
* Kevin Lynx
* 4.11.2010
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sm.h"

/* map to op_type */
const char *op_desc[] = {
	"HALT", "IN", "OUT", "ADD", "SUB", "MUL", "DIV",
	"DUP",
	"LD", "ST", "LDC", "JLT", "JLE", "JGT", "JGE", "JEQ", "JNE", "JMP", 0
};

FILE *fp_in;
FILE *fp_out;

/* get op from its string desc */
int get_op( const char *s )
{
	int i = 0;
	for( ; op_desc[i] != 0; ++i )
	{
		if( strcmp( op_desc[i], s ) == 0 )
		{
			return i;
		}
	}
	return opInvalid;
}

/* get the op code arg(operand) count */
int get_operand_count( int op )
{
	int ret;
	switch( op )
	{
	case opLdc:
	case opJlt:
	case opJle:
	case opJgt:
	case opJge:
	case opJeq:
	case opJne:
	case opJmp:
		ret = 1;
		break;
	default:
		ret = 0;
	}
	return ret;
}

void do_asm()
{
	char line[256];
	char op_str[32];
	unsigned short op;
	int arg_c;
	int arg;
	unsigned short loc;
	while( !feof( fp_in ) )
	{
		fgets( line, sizeof( line ) - 1, fp_in );
		sscanf( line, "%d%s", (int*)&loc, op_str );
		op = (unsigned short) get_op( op_str );
		arg_c = get_operand_count( op );
		/* output */
		fwrite( &loc, sizeof( loc ), 1, fp_out );
		fwrite( &op, sizeof( op ), 1, fp_out );
		if( arg_c > 0 )
		{
			char *s = strstr( line, op_str );
			s = &s[strcspn( s, " \t" )+1];
			arg = atoi( s );
			fwrite( &arg, sizeof( arg ), 1, fp_out );			
		}
	}
}

int main( int argc, char **argv )
{
	if( argc < 2 )
	{
		fprintf( stderr, "Usage:%s <filename>\n", argv[0] );
		exit( -1 );
	}
	fp_in = fopen( argv[1], "r" );
	if( fp_in == 0 )
	{
		fprintf( stderr, "Open %s failed\n", argv[1] );
		exit( -1 );
	}
	{
		char output[256] = { 0 };
		int l = strcspn( argv[1], "." );
		strncpy( output, argv[1], l );
		strcat( output, ".sm" );
		fp_out = fopen( output, "wb" );
		if( fp_out == 0 )
		{
			fprintf( stderr, "Open %s failed\n", output );
			exit( -1 );
		}
	}

	do_asm();

	fclose( fp_in );
	fclose( fp_out );
	return 0;
}

