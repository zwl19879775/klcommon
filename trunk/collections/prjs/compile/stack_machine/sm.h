/**
 * sm.h
 */
#ifndef __SM_H_
#define __SM_H_

enum op_type
{
	opHalt, opIn, opOut, opAdd, opSub, opMul, opDiv,
	opDup,
	opLd, opSt, opLdc, opJlt, opJle, opJgt, opJge, opJeq, opJne, opJmp,
	opInvalid
};

typedef struct Instruction
{
	int op;
	int arg;
} Instruction;

#define CODE_SIZE (1024)
#define DATA_SIZE (1024)

#endif

