/**
 * state.c
 * kevin lynx
 * 2.15.2010
 */
#include "state.h"
#include <stdlib.h>
#include <stdio.h>

#define MALLOC( type ) \
	(type*) malloc( sizeof( type ) )

#ifdef DEBUG
static int id_index = 0;

static int gen_id()
{
	return id_index++;
}
#endif

struct state *state_new()
{
	struct state *s = MALLOC( struct state );
	s->trans = 0;
	s->ref = 0;
#ifdef DEBUG
	s->id = gen_id();
	s->print_flag = 0;
#endif
	return s;
}

void state_free( struct state *s )
{
	struct tran *t;
	if( s->ref-- > 0 )
	{
		return;	
	}
	for( t = s->trans; t != 0; )
	{
		state_free( t->dest );
		struct tran *tmp_t = t;
		t = t->next;
		free( tmp_t );
	}
	free( s );
}

void state_link( struct state *src, struct state *dest, char c )
{
	struct tran *t = MALLOC( struct tran );
	t->dest = dest;
	t->c = c;
	t->next = 0;
	if( src->trans == 0 )
	{
		src->trans = t;
	}
	else
	{
		t->next = src->trans;
		src->trans = t;
	}
	dest->ref++;
}

#ifdef DEBUG
#define VISITED (-1)
void state_print( struct state *s )
{
	struct tran *t;
	if( s->print_flag == VISITED )
	{
		return;
	}
	printf( "\n%d", s->id );
	s->print_flag = VISITED;
	for( t = s->trans; t != 0; t = t->next )
	{
		printf( " -%c-> %d", t->c, t->dest->id );
	}
	for( t = s->trans; t != 0; t = t->next )
	{
		state_print( t->dest );
	}
}
#endif

