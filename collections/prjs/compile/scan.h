/**
 * scan.h
 */
#ifndef ___SCAN_H_
#define ___SCAN_H_

enum
{
	NONE,
	DONE,
	CLOSURE, /* '*' */
	CONCAT, /* '.' */
	UNION, /* '|' */
};

extern void scan_init( const char *str );
int scan_next();
void scan_reverse();
char *scan_get_regc( const char *reg_expr, char *buf );

extern char lex_value;
extern int lex_type;
#endif

