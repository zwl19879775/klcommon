/**
  @file xml_parser.h
  @author Kevin Lynx
*/
#ifndef ___XML_PARSER_H_
#define ___XML_PARSER_H_

/**
  attribute pair
*/
struct xmlAttr
{
	char *name;
	char *value;
	struct xmlAttr *next;
};

/**
  xml node
*/
struct xmlNode
{
	char *name; /* node name */
	struct xmlAttr *attr_list; /* attribute list */
	struct xmlNode *sibling; /* sibling of this node */
	struct xmlNode *children; /* children of this node */
	struct xmlNode *parent; /* parentof this node */
};

/**
  xml document 
*/
struct xmlDocument
{
#ifdef XML_USE_FILE
	FILE *xmlfile; /* the xml input file */
#else
	void *buf;
	size_t size;
#endif
	struct xmlNode *root; /* the root node of this xml file, you can access other nodes by it */
};

/**
  create a new xml document, only malloc the memory.
*/
#ifdef XML_USE_FILE
struct xmlDocument *xmldoc_new( FILE *fp );
#else
struct xmlDocument *xmldoc_new( const void *buf, size_t size );
#endif

/**
  free a xml document.
*/
void xmldoc_free( struct xmlDocument *doc );

/**
  parse a xml document, and you can access these attributes after this process.
*/
int xml_parse( struct xmlDocument *doc );

/** error log function prototype */
typedef void (*xml_errorfn)( unsigned long lineno, const char *msg );

void xml_seterrorfn( xml_errorfn fn );
#endif 
