/**
  @file xml_scan.h
  @author Kevin Lynx 
*/
#ifndef ___XML_SCAN_H_
#define ___XML_SCAN_H_

#define TOKEN_LENGTH 512

/**
  token type
*/
typedef enum 
{
	ENDFILE = 0, /* yylex will return 0 when scanned finished */
	ERROR,
	LB, /* < */
	RB, /* > */
	EE, /* /> */
	EBE, /* </ */
	ID,
	ASSIGN,
	VALUE
} TokenType;

/**
  token 
*/
struct Token
{
	TokenType type;
	char string[TOKEN_LENGTH];
};

/** 
  scan the file and return the current token
*/
struct Token xmlscan_gettoken();

/**
  retrieve the current scan line.
*/
unsigned long xmlscan_line();

/**
  init scanning setttings
*/
void xmlscan_init( FILE *infile );

/** a helper macro to create objects */
#define NEW( type ) (type*) malloc( sizeof( type ) )
#define FREE( obj ) free( obj )

#endif 
