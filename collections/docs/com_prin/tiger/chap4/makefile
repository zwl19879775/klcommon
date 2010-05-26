parsetest: parsetest.o y.tab.o lex.yy.o errormsg.o util.o absyn.o parse.o \
	symbol.o table.o prabsyn.o
	cc -g -o $@ $^

prabsyn.o: prabsyn.c prabsyn.h absyn.h
	cc -g -c $<

absyn.o: absyn.c absyn.h
	cc -g -c $<

parse.o: parse.c parse.h
	cc -g -c $<

symbol.o: symbol.c symbol.h table.h
	cc -g -c $<

table.o: table.c table.h
	cc -g -c $<

parsetest.o: parsetest.c errormsg.h util.h
	cc -g -c parsetest.c

y.tab.o: y.tab.c
	cc -g -c y.tab.c

y.tab.c: tiger.grm
	yacc -dv tiger.grm

y.tab.h: y.tab.c
	echo "y.tab.h was created at the same time as y.tab.c"

errormsg.o: errormsg.c errormsg.h util.h
	cc -g -c errormsg.c

lex.yy.o: lex.yy.c y.tab.h errormsg.h util.h
	cc -g -c lex.yy.c

lex.yy.c: tiger.lex
	lex tiger.lex

util.o: util.c util.h
	cc -g -c util.c

clean: 
	rm -f util.o parsetest.o lex.yy.o errormsg.o y.tab.c y.tab.h y.tab.o \
		absyn.o parse.o symbol.o table.o parsetest prabsyn.o
