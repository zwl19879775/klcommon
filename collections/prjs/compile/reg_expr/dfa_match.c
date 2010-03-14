/**
 * dfa_match.c
 * convert NFA to DFA first, and use the DFA to match string
 * the DFA may has many final states while the NFA has only 1 final state.
 * kevin lynx
 * 2.16.2010
 */
#include "state_oper.h"
#include "scan.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

extern struct state_pair parse( const char *reg_expr );

/* map a state to a state-list, also represent DStates */
struct state_map
{
	struct state *s;
	struct state_list *slist;
	struct state_map *next;
	int mark;
};

/* DFA has 1 start state and many final states */
struct state_set
{
	struct state *start;
	struct state_list *finals;
};

static void dstate_add( struct state_map **smap, struct state_list *slist )
{
	struct state_map *item = (struct state_map*) malloc( sizeof( struct state_map ) );
	item->s = state_new();
	item->slist = slist;
	item->mark = 0;
	item->next = *smap;
	*smap = item;	
}

static void dstate_free( struct state_map *smap )
{
	while( smap != 0 )
	{
		struct state_map *item = smap;
		smap = smap->next;
		slist_free( item->slist );
		/* item->s will  be handled by user */
		free( item );
	}
}

static void dstate_mark( struct state_map *smap, const struct state_list *slist )
{
	for( ; smap != 0; smap = smap->next )
	{
		if( slist == smap->slist )
		{
			smap->mark = 1;
		}
	}
}

static struct state_list *dstate_get_unmark( struct state_map *smap )
{
	for( ; smap != 0; smap = smap->next )
	{
		if( smap->mark == 0 )
		{
			return smap->slist;
		}
	}
	return 0;
}

static int slist_equal( const struct state_list *left, const struct state_list *right )
{
	for( ; left != 0 && right != 0; left = left->next, right = right->next )
	{
		if( left->s != right->s )
		{
			return 0;
		}
	}
	/* length is not equal */
	if( left != right )
	{
		return 0;
	}
	return 1;
}

static int slist_has_state( const struct state_list *slist, const struct state *s )
{
	for( ; slist != 0; slist = slist->next )
	{
		if( slist->s == s )
		{
			return 1;
		}
	}
	return 0;
}

static struct state_list *dstate_is_in( const struct state_map *smap, const struct state_list *slist )
{
	for( ; smap != 0; smap = smap->next )
	{
		if( slist_equal( smap->slist, slist ) )
		{
			return smap->slist;
		}
	}
	return 0;
}

static struct state *dstate_query_state( struct state_map *smap, const struct state_list *slist )
{
	for( ; smap != 0; smap = smap->next )
	{
		if( smap->slist == slist )
		{
			return smap->s;
		}
	}
	return 0;
}

struct state_set dfa_convert( struct state_pair *nfa, const char *reg_char )
{
	struct state_set ss = { 0 };
	struct state_list *slist = 0;
	struct state_map *smap = 0;
	const char *reg_s = 0;
	soper_e_closure_s( nfa->start, &slist );
	dstate_add( &smap, slist );
	ss.start = dstate_query_state( smap, slist );
	/* the start state maybe the final state */
	if( slist_has_state( slist, nfa->final ) )
	{
		slist_add( &ss.finals, ss.start );
	}
	while( ( slist = dstate_get_unmark( smap ) ) != 0 )
	{
		dstate_mark( smap, slist );
		for( reg_s = reg_char; *reg_s != '\0'; reg_s ++ )
		{
			struct state_list *loc_slist = 0;
			struct state_list *loc_slist2 = 0;
			struct state_list *loc_slist3 = 0;
			soper_move( slist, &loc_slist, *reg_s );
			soper_e_closure( loc_slist, &loc_slist2 );
			if( loc_slist != 0 )
			{
				slist_free( loc_slist );
			}
			if( loc_slist2 == 0 ) /* no state */
			{
				continue;
			}
			if( ( loc_slist3 = dstate_is_in( smap, loc_slist2 ) ) == 0 )
			{
				dstate_add( &smap, loc_slist2 );
			}
			else
			{
				slist_free( loc_slist2 );
				loc_slist2 = loc_slist3;
			}
			struct state *src = dstate_query_state( smap, slist );
			struct state *dest = dstate_query_state( smap, loc_slist2 );
			state_link( src, dest, *reg_s );

			if( slist_has_state( loc_slist2, nfa->final ) &&
				!slist_has_state( ss.finals, dest ) )
			{
				slist_add( &ss.finals, dest );
			}
		}
	} 

	dstate_free( smap );
	return ss;
}

static int dfa_do_match( struct state *s, const char *str, struct state_list *finals )
{
	struct tran *t = s->trans;
	if( slist_has_state( finals, s ) )
	{
		return 1;
	}
	if( *str == '\0' )
	{
		return 0;
	}
	for( ; t != 0; t = t->next )
	{
		if( t->c == *str )
		{
			return dfa_do_match( t->dest, str + 1, finals );
		}
	}
	return 0;
}

int dfa_match( const char *reg_expr, const char *str )
{
	/* get the NFA */
	struct state_pair sp = parse( reg_expr );
	struct state_set ss;
	char *reg_char = (char*) malloc( strlen( str ) + 1 );
	reg_char[0] = 0;
	scan_get_regc( reg_expr, reg_char );
#ifdef DEBUG
	state_print( sp.start );
	printf( "\n" );
#endif
	/* convert it to DFA */
	ss = dfa_convert( &sp, reg_char );

#ifdef DEBUG
	printf( "start:%d\n", ss.start->id );
	printf( "finals:" );
	struct state_list *slist = ss.finals;
	for( ; slist != 0; slist = slist->next )
	{
		printf( "%d ", slist->s->id );
	}
	state_print( ss.start );
#endif
	/* match the str in this DFA */
	int ret = dfa_do_match( ss.start, str, ss.finals );

	state_free( ss.start );
	free( reg_char );
	return ret;
}

