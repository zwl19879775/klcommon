/**
 * code.h
 */
#ifndef __CODE_H_
#define __CODE_H_

void emitCode( int op );
void emitCodeArg( int op, int arg );
int emitSkip( int how_many );
void emitBackup( int l );
void emitRestore();
int emitDone( const char *file );

#endif

