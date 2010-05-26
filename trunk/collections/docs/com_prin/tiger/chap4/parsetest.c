/*
 * added by Kevin Lynx 5.26.2010
 */
#include <stdio.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "parse.h"
#include "prabsyn.h"

int main(int argc, char **argv) {
 if (argc!=2) {fprintf(stderr,"usage: a.out filename\n"); exit(1);}
 {
	 A_exp root=parse(argv[1]);
	 pr_exp( stdout, root, 4 );
 }
 return 0;
}
