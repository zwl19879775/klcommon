/**
  @file xml_helper.c
  @brief to provide some helper functions/macros to visit the properties
  @author Kevin Lynx
*/
#include <stdio.h>
#include "xml_helper.h"

#define ACCESS_ATTRIBUTE( node, name, format, value ) \
	const char *strv = xml_attrstring( node, name ); \
	if( strv == 0 ) \
	{ \
		return -1; \
	} \
	if( sscanf( strv, format, value ) == 1 ) \
	{ \
		return 0; \
	} \
	return -1

const char *xml_attrstring( const struct xmlNode *node, const char *name )
{
	struct xmlAttr *attr = node->attr_list;
	while( attr != 0 )
	{
		if( strcmp( attr->name, name ) == 0 )
		{
			return attr->value;
		}
		attr = attr->next;
	}

	return 0;
}

int xml_attrint( const struct xmlNode *node, const char *name, int *value )
{
	ACCESS_ATTRIBUTE( node, name, "%d", value );
}

int xml_attrfloat( const struct xmlNode *node, const char *name, float *value )
{
	ACCESS_ATTRIBUTE( node, name, "%f", value );
}

int xml_attrdouble( const struct xmlNode *node, const char *name, double *value )
{
	ACCESS_ATTRIBUTE( node, name, "%lf", value );
}

struct xmlNode *xml_getchild( const struct xmlNode *parent, const char *name )
{
	struct xmlNode *node = parent->children;
	while( node != 0 )
	{
		if( strcmp( node->name, name ) == 0 )
		{
			return node;
		}
		node = node->sibling;
	}

	return node;
}



