/**
 * Kevin Lynx
 * 5.31.2010
 */
#include "env.h"

E_enventry E_VarEntry( Ty_ty ty )
{
	E_enventry v = checked_malloc(sizeof(*v));
	v->kind = E_varEntry;
	v->u.var.ty = ty;
	return v;
}

E_enventry E_FunEntry( Ty_tyList formals, Ty_ty result )
{
	E_enventry v = checked_malloc(sizeof(*v));
	v->kind = E_funEntry;
	v->u.fun.formals = formals;
	v->u.fun.result = result;
	return v;
}

S_table E_base_tenv(void)
{
	static S_table tenv = 0;
	if( !tenv ) tenv = TAB_empty();
	return tenv;
}

S_table E_base_venv(void)
{
	static S_table venv = 0;
	if( !venv ) venv = TAB_empty();
	return venv;
}


