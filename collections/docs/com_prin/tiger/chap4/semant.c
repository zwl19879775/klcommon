/**
 * Kevin Lynx
 * 5.31.2010
 */
#include "semant.h"
#include "errormsg.h"


struct expty transVar( S_table venv, S_table tenv, A_var v )
{
	switch(v->kind)
	{
	case A_simpleVar: 
		{
			E_enventry x = S_look(venv,v->u.simple);
			if( x && x->kind == E_varEntry )
				return expTy( 0, x->u.var.ty );
			else
			{
				EM_error(v->pos, "undefined variable %s", 
						S_name(v->u.simple));
				return expTy(0, Ty_Int());
			}
		}
		break;
	default:
		break;
	}
	return expTy(0, Ty_Int());
}

struct expty transExp( S_table venv, S_table tenv, A_exp a )
{
	switch(a->kind)
	{
	case A_letExp:
		{
			struct expty exp;
			A_decList d;
			S_beginScope( venv );
			S_beginScope( tenv );
			for( d = a->u.let.decs; d; d=d->tail )
				transDec( venv, tenv, d->head );
			exp = transExp( venv, tenv, a->u.let.body );
			S_endScope( tenv );
			S_endScope( venv );
			return exp;
		}
		break;
	case A_varExp:
		{
			struct expty exp;
			exp = transVar( venv, tenv, a->u.var );
			return exp;
		}
		break;
	case A_seqExp:
		{
			A_expList exps = a->u.seq;
			struct expty exp;
			for( ; exps; exps = exps->tail )
			{
				exp = transExp( venv, tenv, exps->head );
			}
			return exp;
		}
		break;
	default:
		break;
	}
	return expTy(0, Ty_Int());
}

void transDec( S_table venv, S_table tenv, A_dec d )
{
	switch(d->kind)
	{
	case A_varDec:
		{
			struct expty e = transExp(venv, tenv, d->u.var.init);
			if( d->u.var.typ != 0 )
			{
				TType t = (TType)S_look(tenv, d->u.var.typ);
				if( t == 0 )
					EM_error( d->pos, "undefined type %s", S_name( d->u.var.typ ) );
				/* TODO: check nil */
				if( t->kind != e.ty->kind )
					EM_error( d->pos, "unmatched type");
			}
			else
			{
				S_enter(venv, d->u.var.var, E_VarEntry(e.ty));	
			}
		}
		break;
	default:
		break;
	}
}

struct expty expTy( Tr_exp exp, Ty_ty ty )
{
	struct expty e;
	e.exp = exp; e.ty = ty;
	return e;
}

void SEM_transProg( A_exp exp )
{
	S_table tenv = E_base_tenv();
	S_table venv = E_base_venv();
	transExp( venv, tenv, exp );
}

