/**
 * generate code on SM
 * Kevin Lynx
 * 4.12.2010
 */
#include "../sm.h"
#include "code.h"
#include <stdio.h>

static Instruction i_mem[CODE_SIZE];
static int loc = 0;
static int high_loc = 0;

#define INC_HIGH_LOC if( high_loc < loc ) high_loc = loc

/* get the op code arg(operand) count */
static int get_arg_count( int op )
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

void emitCode( int op )
{
	i_mem[loc++].op = op;
	INC_HIGH_LOC;
}

void emitCodeArg( int op, int arg )
{
	i_mem[loc].op = op;
	i_mem[loc++].arg = arg;
	INC_HIGH_LOC;
}

void emitCodeRel( int op, int relLoc )
{
	emitCodeArg( op, loc + relLoc + 1 );
}

int emitSkip( int how_many )
{
	int i = loc;
	loc += how_many;
	INC_HIGH_LOC;
	return i;
}

void emitBackup( int l )
{
	loc = l;
}

void emitRestore()
{
	loc = high_loc;
}

int emitDone( const char *file )
{
	FILE *fp = fopen( file, "wb" );
	int i;
	if( fp == 0 )
	{
		return -1;
	}
	for( i = 0; i <= loc; ++ i )
	{
		char op = (char) i_mem[i].op;
		int arg = i_mem[i].arg;
		fwrite( &op, sizeof( op ), 1, fp ); /*op 1 byte */
		if( get_arg_count( op ) > 0 )
		{
			fwrite( &arg, sizeof( arg ), 1, fp ); /*arg 4 byte */
		}
	}	
	fclose( fp );
	return 0;
}

