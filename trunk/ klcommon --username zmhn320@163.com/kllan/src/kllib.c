/**
 * @file kllib.h
 * @author Kevin Lynx
 * @brief handle kl-addons etc
 */
#include "kllib.h"
#include "klsymtab.h"
#include "klinterpret.h"
#include "klparser.h"
#include "kllex.h"
#include <stdio.h>
#include <stdarg.h>

struct klState *kl_new( kl_log l )
{
	struct klState *kl = (struct klState*) malloc( sizeof( struct klState ) );
	kl->log = l;
	kl->st = sym_new();
	return kl;
}

void kl_free( struct klState *kl )
{
	sym_free( kl->st );
	free( kl );
}

int kl_register( struct klState *kl, kl_func f, const char *name )
{
	struct Value val ;
	val.address = f;
	val.type = SB_FUNC;
	sym_insert( kl->st, name, val );
	return 0;
}

int kl_run( struct klState *kl, char *source )
{
	struct lexState ls;
	struct treeNode *tree;
	int t;
	lex_setinput( &ls, source, kl->log );
	tree = syn_parse( &ls );
	inter_execute( tree, kl->log, kl->st );
	syn_free_tree( tree );
	return 0;
}
