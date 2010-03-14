/**
 * state_oper.h
 * some operations on NFA state
 * kevin lynx
 * 2.15.2010
 */
#ifndef ___STATE_OPER_H_
#define ___STATE_OPER_H_

#include "state.h"

/* actually it's a state list item */
struct state_list
{
	struct state *s;
	struct state_list *next;
};

struct state_list *slist_new();

void slist_free( struct state_list *slist );

void slist_add( struct state_list **slist, struct state *s );

void soper_e_closure( const struct state_list *slist, struct state_list **ret_list );

void soper_e_closure_s( const struct state *s, struct state_list **ret_list );

void soper_move( const struct state_list *slist, struct state_list **ret_list, char c );

void soper_move_s( const struct state *s, struct state_list **ret_list, char c );

#endif

