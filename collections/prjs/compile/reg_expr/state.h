/**
 * state.h
 * state definition for FAs(DFA or NFA)
 * kevin lynx
 * 2.15.2010
 */
#ifndef ___STATE_H_
#define ___STATE_H_

struct state;

#define E_TOK (0)

/* transition */
struct tran
{
	char c;
	struct state *dest;
	struct tran *next;
};

struct state
{
	/* a list of transitions */
	struct tran *trans;
	/* inc when be linked */
	int ref;
#ifdef DEBUG
	int id;
	int print_flag;
#endif
};

struct state_pair
{
	struct state *start;
	struct state *final;
};

struct state *state_new();

void state_free( struct state *s );

void state_link( struct state *src, struct state *dest, char c );

#ifdef DEBUG
void state_print( struct state *s );
#endif

#endif

