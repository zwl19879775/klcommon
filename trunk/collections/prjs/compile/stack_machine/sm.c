/**
 * A tiny stack machine only for compilier practice
 * Kevin Lynx
 * 4.10.2010
 */
#include <stdio.h>
#include <stdlib.h>
#include "sm.h"

#define STACK_SIZE (256)
#define POP_ERR (0x7fffffff)

/* the operation stack */
int op_stack[STACK_SIZE];
int op_pos = 0;

Instruction i_mem[CODE_SIZE];
int pc;

int d_mem[DATA_SIZE];

enum err_code
{
	Halt, Okay, errDivByZero, errDMem, errIMem, errStackOverflow, errStackEmpty, 
	errUnknownOp
};

void error( const char *err )
{
	fprintf( stderr, err );
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

int push_op_stack( int i )
{
	if( op_pos >= STACK_SIZE )
	{
		error( "stack overflow" );
		return -1;
	}
	op_stack[op_pos++] = i;
	return 0;
}

int pop_op_stack()
{
	if( op_pos == 0 )
	{
		error( "stack empty" );
		return POP_ERR;
	}
	return op_stack[--op_pos];
}

int top_op_stack()
{
	if( op_pos == 0 )
	{
		error( "stack empty" );
		return POP_ERR;
	}
	return op_stack[op_pos-1];
}

#define INC_P( t ) codes+=sizeof(t); size-=sizeof(t)
/* read codes into the i_mem */
int read_instruction( const char *codes, int size )
{
	int op_count, loc = 0;
	Instruction inst;
	while( size > 0 && loc < CODE_SIZE )
	{
		/* op is 1 byte in the code file */
		inst.op = *codes;	
		INC_P( char );
		op_count = get_operand_count( inst.op );
		if( op_count > 0 ) /* has arg */
		{
			inst.arg = *(int*) codes;
			INC_P( int );
		}
		else
		{
			inst.arg = 0;
		}
		i_mem[loc++] = inst;
	}
	return 1;
}

int step_run()
{
	Instruction *inst = &i_mem[pc++];
	int ret = Okay;;
	switch( inst->op )
	{
	case opHalt:
		{
			ret = Halt;
		}
		break;
	case opIn:
		{
			int i;
			scanf( "input:%d", &i );
			push_op_stack( i );
		}
		break;
	case opOut:
		{
			int i = pop_op_stack();
			printf( "output:%d\n", i );
		}
		break;
	case opAdd:
		{
			int a = pop_op_stack();
			int b = pop_op_stack();
			push_op_stack( b + a );
		}
		break;
	case opSub:
		{
			int a = pop_op_stack();
			int b = pop_op_stack();
			push_op_stack( b - a );
		}
		break;
	case opMul:
		{
			int a = pop_op_stack();
			int b = pop_op_stack();
			push_op_stack( b * a );
		}
		break;
	case opDiv:
		{
			int a = pop_op_stack();
			int b = pop_op_stack();
			if( a == 0 )
			{
				return errDivByZero;
			}
			push_op_stack( b / a );
		}
		break;
	case opDup:
		{
			push_op_stack( top_op_stack() );
		}
		break;
	case opLd:
		{
			int addr = pop_op_stack();
			if( addr < 0 || addr >= DATA_SIZE )
			{
				error( "data memory access error" );
				return errDMem;
			}
			else
			{
				push_op_stack( d_mem[addr] );
			}
		}
		break;
	case opSt:
		{
			int val = pop_op_stack();
			int addr = pop_op_stack();
			if( addr < 0 || addr >= DATA_SIZE )
			{
				error( "data memory access error" );
				return errDMem;
			}
			else
			{
				d_mem[addr] = val;
			}	
		}
		break;
	case opLdc:
		{
			push_op_stack( inst->arg );
		}
		break;
	case opJlt: 
		{
			int i = pop_op_stack();
			if( i < 0 ) 
			{
				pc = inst->arg;
			}
		}
		break;
	case opJle: 
		{
			int i = pop_op_stack();
			if( i <= 0 ) 
			{
				pc = inst->arg;
			}
		}
		break;
	case opJgt: 
		{
			int i = pop_op_stack();
			if( i > 0 ) 
			{
				pc = inst->arg;
			}
		}
		break;
	case opJge: 
		{
			int i = pop_op_stack();
			if( i >= 0 ) 
			{
				pc = inst->arg;
			}
		}
		break;
	case opJeq: 
		{
			int i = pop_op_stack();
			if( i == 0 ) 
			{
				pc = inst->arg;
			}
		}
		break;
	case opJne: 
		{
			int i = pop_op_stack();
			if( i != 0 ) 
			{
				pc = inst->arg;
			}
		}
		break;
	case opJmp:
		{
			pc = inst->arg;
		}
		break;
	default:
		ret = errUnknownOp;
	}
	return ret;
}

void run()
{
	int ret = Okay;
	while( ret == Okay )
	{
		ret = step_run();
	}
}

int file_size( FILE *fp )
{
	int size;
	fseek( fp, 0, SEEK_SET );
	fseek( fp, 0, SEEK_END );
	size = ftell( fp );
	fseek( fp, 0, SEEK_SET );
	return size;
}

int main( int argc, char **argv )
{
	FILE *fp;
	char *buf;
	int size;
	if( argc < 2 )
	{
		error( "Usage: SM <filename>" );
		exit( -1 );
	}
	fp = fopen( argv[1], "rb" );
	if( fp == 0 )
	{
		error( "Open file failed" );
		exit( -1 );
	}
	size = file_size( fp );
	buf = (char*) malloc( size );
	fread( buf, size, 1, fp );

	read_instruction( buf, size );
	run();

	free( buf );
	fclose( fp );
	return 0;
}

