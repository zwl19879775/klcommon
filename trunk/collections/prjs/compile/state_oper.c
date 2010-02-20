/**
 * state_oper.c
 * some operations on NFA state
 * kevin lynx
 * 2.15.2010
 */
#include "state_oper.h"
#include <stdlib.h>

#define MALLOC( type ) \
	(type*) malloc( sizeof( type ) )

struct state_list *slist_new()
{
	struct state_list *slist = MALLOC( struct state_list );
	slist->s = 0;
	slist->next = 0;
	return slist;
}

void slist_free( struct state_list *slist )
{
	struct state_list *item;
	for( item = slist; item != 0;  )
	{
		struct state_list *tmp = item;
		item = item->next;
		free( tmp );
	}
}

void slist_add( struct state_list **slist, struct state *s )
{
	struct state_list *item = slist_new();
	item->s = s;
	item->next = *slist;
	*slist = item;
}

void soper_e_closure( const struct state_list *slist, struct state_list **ret_list )
{
	for( ; slist != 0; slist = slist->next )
	{
		soper_e_closure_s( slist->s, ret_list );
	}
}

void soper_e_closure_s( const struct state *s, struct state_list **ret_list )
{
	/* add self */
	slist_add( ret_list, (struct state*) s );
	const struct tran *t = s->trans;
	for( ; t != 0; t = t->next )
	{
		if( t->c == E_TOK )
		{
			/* E-closure should recursively */
			soper_e_closure_s( t->dest, ret_list );
		}
	}
}

void soper_move( const struct state_list *slist, struct state_list **ret_list, char c )
{
	for( ; slist != 0; slist = slist->next )
	{
		soper_move_s( slist->s, ret_list, c );
	}	
}

void soper_move_s( const struct state *s, struct state_list **ret_list, char c )
{
	const struct tran *t = s->trans;
	for( ; t != 0; t = t->next )
	{
		if( t->c == c )
		{
			slist_add( ret_list, t->dest );
		}
	}
}

