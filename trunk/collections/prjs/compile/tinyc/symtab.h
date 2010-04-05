
#ifndef ___SYMTAB_H_
#define ___SYMTAB_H_

int sym_lookup( const char *name );
void sym_insert( const char *name, int loc );
void sym_clear();
void sym_dump();

#endif

