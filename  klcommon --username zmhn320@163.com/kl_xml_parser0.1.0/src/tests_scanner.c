/**
  to test the scanner
*/
#include <stdio.h>
#include "xml_scan.h"


void print_token( struct Token token )
{
	switch( token.type )
	{
		case LB:
			printf( "LB\n" );
			break;
		case RB:
			printf( "RB\n" );
			break;
		case EE:
			printf( "EE\n" );
			break;
		case ID:
			printf( "ID : %s\n", token.string );
			break;
		case EBE:
			printf( "EBE\n" );
			break;
		case VALUE:
			printf( "VALUE : %s\n", token.string );
			break;
		case ASSIGN:
			printf( "ASSIGN\n" );
			break;
		default:
			printf( "Scan error at line #%u\n", xmlscan_line() );
	}
}

int main( int argc, char **argv )
{
	FILE *fp = 0;
	if( argc < 2 )
	{
		fprintf( stderr, "Usage : %s filename.\n", argv[0] );
		exit( -1 );
	}

	fp = fopen( argv[1], "r" );
	if( fp == NULL )
	{
		fprintf( stderr, "Unable to open %s.\n", argv[1] );
		exit( -1 );
	}

	{
		xmlscan_init( fp );
		struct Token token = xmlscan_gettoken();
   		while( token.type != ENDFILE )
		{
			print_token( token );
			token = xmlscan_gettoken();
		}
	}
	
	fclose( fp );
	return 0;
}
