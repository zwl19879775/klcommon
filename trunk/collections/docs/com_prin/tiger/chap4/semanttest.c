/*
 * added by Kevin Lynx 5.31.2010
 */
#include <stdio.h>
#include <stdlib.h>
#include "semant.h"
#include "parse.h"
#include "prabsyn.h"

int main(int argc, char **argv) {
 if (argc!=2) {fprintf(stderr,"usage: a.out filename\n"); exit(1);}
 {
	 A_exp root=parse(argv[1]);
	 pr_exp( stdout, root, 4 );
	 SEM_transProg( root );
 }
 return 0;
}

