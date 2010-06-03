
#include "util.h"
#include "table.h"
#include "symbol.h"
#include "types.h"

typedef struct E_enventry_ *E_enventry;

struct E_enventry_ {
	enum { E_varEntry, E_funEntry } kind;
	union { 
		struct { Ty_ty ty; } var;
		struct { Ty_tyList formals; Ty_ty result; } fun;
	} u;
};

E_enventry E_VarEntry( Ty_ty ty );
E_enventry E_FunEntry( Ty_tyList formals, Ty_ty result );

S_table E_base_tenv(void);
S_table E_base_venv(void);

typedef E_enventry VType; /* item type in value env table */
typedef Ty_ty TType; /* item type in type env table */

