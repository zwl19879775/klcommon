/**
  @file xml_parser.c
  @brief to parse the simple xml file
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xml_scan.h"
#include "xml_parser.h"

static xml_errorfn errorfn;

#define ERROR_LOG( msg ) \
{ if( errorfn != 0 ) errorfn( xmlscan_line(), msg ); }

/** free attributes */
static void _free_attr( struct xmlAttr *attr )
{
	if( attr == 0 )
	{
		return;
	}
	free( attr->name );
	free( attr->value );
	_free_attr( attr->next );
	free( attr );
}

/** free nodes */
static void _free_node( struct xmlNode *node )
{
	if( node == 0 )
	{
		return ;
	}
	free( node->name );
	/* free attribute */
	_free_attr( node->attr_list );
	/* free children node */
	_free_node( node->children );
	/* free sibling node */
	_free_node( node->sibling );
	/* free self */
	free( node );	
}

/* malloc a new node */
static struct xmlNode *_new_node()
{
	struct xmlNode *node = NEW( struct xmlNode );
	node->name = 0;
	node->sibling = 0;
	node->children = 0;
	node->parent = 0;
	node->attr_list = 0;
	return node;
}

/* malloc a new attribute pair */
static struct xmlAttr *_new_attr()
{
	struct xmlAttr *attr = NEW( struct xmlAttr );
	attr->name = 0;
	attr->value = 0;
	attr->next = 0;
	return attr;
}

/** return RB or EE to identify the match result */
static int _match_attribute( struct xmlNode *node )
{
	struct xmlAttr *attr = 0;
	struct Token token = xmlscan_gettoken();
	for( ; token.type != RB && token.type != EE; token = xmlscan_gettoken() )
	{
		if( token.type == ERROR || token.type == ENDFILE )
		{
			ERROR_LOG( "expected '>' or '/>' for node" );
			return ERROR;
		}

		if( token.type == ID )
		{
			attr = _new_attr();
			attr->name = (char*) malloc( strlen( token.string ) + 1 ) ;
			strcpy( attr->name, token.string );
			token = xmlscan_gettoken();
			if( token.type != ASSIGN )
			{
				ERROR_LOG( "expected '=' for attribute" );
				free( attr->name );
				free( attr );
				return ERROR ;
			}
			token = xmlscan_gettoken();
			if( token.type != VALUE )
			{
				ERROR_LOG( "expected value for attribute" );
				free( attr->name );
				free( attr );
				return ERROR;
			}
			attr->value = (char*)malloc( strlen( token.string ) + 1 );
			strcpy( attr->value, token.string );			

			/* link the attribute pair */
			attr->next = node->attr_list;
			node->attr_list = attr;
		}
		else
		{
			ERROR_LOG( "error token when matching attributes" );
			return ERROR;
		}
	}
	return token.type;
}
	
static struct xmlNode *_match_node( struct xmlNode *parent, struct xmlNode *prev )
{
	struct xmlNode *node = 0;
	struct Token token = xmlscan_gettoken();
	if( token.type == ENDFILE || token.type == ERROR )
	{
		return 0;
	}
	else if( token.type == ID )
	{
		int ret = 0;
		node = _new_node();
		node->name = (char*) malloc( strlen( token.string ) + 1 );
		strcpy( node->name, token.string );

		/* link to the parent or the previous node */
		if( parent != 0 && parent->children == 0 )
		{
			parent->children = node;
			node->parent = parent;
		}
		else if( prev != 0 )
		{
			prev->sibling = node;
			node->parent = prev->parent;
		}

		/* attributes */
		ret = _match_attribute( node );
		if( ret == RB )
		{
			token = xmlscan_gettoken();
			if( token.type == LB )
			{
				/* recur children nodes */
				_match_node( node, 0 );
			}
			else if( token.type == EBE )
			{
				token = xmlscan_gettoken();
				if( token.type == ID )
				{
					if( strcmp( token.string, node->name ) == 0 )
					{
						/* end this node */
						token = xmlscan_gettoken();
						if( token.type != RB )
						{
							ERROR_LOG( "expect '>'" );
						}
						token = xmlscan_gettoken();
						if( token.type == LB )
						{
							/* recur sibling nodes */
							_match_node( parent, node );
						}
					}
				}
			}
		}
		else if( ret == EE )
		{
			token = xmlscan_gettoken();
			if( token.type == LB )
			{
				/* recur sibliing nodes */
				_match_node( parent, node );
			}
			else if( token.type == EBE )
			{
				token = xmlscan_gettoken();
				if( token.type == ID )
				{
					if( strcmp( token.string, parent->name ) == 0 )
					{
						/* parent node ends */
						token = xmlscan_gettoken();
						if( token.type == LB )
						{
							/* recur sibling nodes */
							_match_node( parent->parent, parent );
						}
					}
				}
			}
		}
		else 
		{
			return 0;
		}
	}

	return node;
}

#ifdef XML_USE_FILE
struct xmlDocument *xmldoc_new( FILE *fp )
{
	struct xmlDocument *doc = NEW( struct xmlDocument );
	doc->xmlfile = fp;
	doc->root = 0;
	return doc;
}
#else
struct xmlDocument *xmldoc_new( const void *buf, size_t size )
{
	struct xmlDocument *doc = NEW( struct xmlDocument );
	doc->buf = malloc( size );
	doc->size = size;
	memcpy( doc->buf, buf, size );
	doc->root = 0;
	return doc;
}
#endif

void xmldoc_free( struct xmlDocument *doc )
{
	_free_node( doc->root );
#ifndef XML_USE_FILE
	free( doc->buf );
#endif
	free( doc );
}

int xml_parse( struct xmlDocument *doc )
{
	struct Token token;
#ifdef XML_USE_FILE
	xmlscan_init( doc->xmlfile );
#else
	xmlscan_init( doc->buf, doc->size );
#endif
	token = xmlscan_gettoken();
	if( token.type != LB )
	{
		ERROR_LOG( "expect '<'" );
		return ERROR;
	}
	doc->root = _match_node( 0, 0 );
	return 0;	
}

void xml_seterrorfn( xml_errorfn fn )
{
	errorfn = fn;
}
