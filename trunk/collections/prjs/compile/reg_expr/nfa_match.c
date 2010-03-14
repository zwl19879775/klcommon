/**
 * nfa_match.c
 * use NFA to match a string
 * kevin lynx
 * 2.15.2010
 */
#include "state_oper.h"

extern struct state_pair parse( const char *reg_expr );

int nfa_match( const char *reg_expr, const char *str )
{
	struct state_pair sp = parse( reg_expr );
	struct state_list *slist = 0;	
	char c;
	int ret = 0;

#ifdef DEBUG
	state_print( sp.start );
#endif

	soper_e_closure_s( sp.start, &slist );
	for( c = str[0]; c != '\0'; c = *(++str) )
	{
		/* not effecitive but worked */
		struct state_list *loc_slist = 0;
		struct state_list *loc_slist2 = 0;
		soper_move( slist, &loc_slist, c );
		soper_e_closure( loc_slist, &loc_slist2 );
		slist_free( slist );
		slist_free( loc_slist );
		slist = loc_slist2;
	}
	for( ; slist != 0; slist = slist->next )
	{
		if( slist->s == sp.final )
		{
			ret = 1;
			break;
		}
	}
	slist_free( slist );
	return ret;
}
	
