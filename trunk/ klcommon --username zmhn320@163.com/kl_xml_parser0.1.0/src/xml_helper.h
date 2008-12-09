/**
  @file xml_helper.h
  @brief to provide some helper functions/macros to visit the properties
  @author Kevin Lynx
*/
#ifndef ___XML_HELPER_H_
#define ___XML_HELPER_H_

#include "xml_parser.h"

/**
  Get the root element.
  @param doc A xml document parsed already.
*/
#define xml_root( doc ) doc->root

/**
  Get the first child of a node.
  @param node the node to query
  @return the first child of the node, 0 if there isn't
*/
#define xml_firstchild( node ) node->children

/**
  Get the next sibling node of a node.
  @param node the node to query
  @return the next sibling node if there is
*/
#define xml_nextsibling( node ) node->sibling

/**
  Get the specified child node by its name
*/
struct xmlNode *xml_getchild( const struct xmlNode *parent, const char *name );

/**
  Query the raw string value by its name
  @param node 
  @param name the attribute name
  @return the string, 0 indicates error
*/
const char *xml_attrstring( const struct xmlNode *node, const char *name );

/**
  Query a value by its name

  @return -1 indicates failure, 
*/
int xml_attrint( const struct xmlNode *node, const char *name, int *value );
int xml_attrfloat( const struct xmlNode *node, const char *name, float *value );
int xml_attrdouble( const struct xmlNode *node, const char *name, double *value );

#endif 
