
#ifndef __SYM_TABLE_H
#define __SYM_TABLE_H

#ifndef MAX_TYPE
#define MAX_TYPE (256)
#endif
#ifndef MAX_SYM
#define MAX_SYM (256)
#endif

int type_register_struct( const char *desc );
int type_register_anon();
int type_register( const char *desc );
int type_get_id( const char *desc );
const char *type_get_desc( int id );
void type_init();
void type_release();
void type_dump();

int sym_add( const char *name, int type );
void sym_clear();
int sym_type( const char *name );
void sym_dump();

#endif

