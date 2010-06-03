# makefile for kl script core
CC = gcc
LINK = gcc
override CFLAG += -c -Wall -o
LINKFLAG = -o
MMFLAG = -MM
RM = rm 
BASESRC = absyn.c parse.c prabsyn.c symbol.c table.c errormsg.c util.c \
		  lex.yy.c y.tab.c
PARSESRC = $(BASESRC) parsetest.c
SEMANTSRC = $(BASESRC) semanttest.c env.c semant.c types.c
ALLSRC = $(BASESRC) parsetest.c semanttest.c
PARSEOBJ = $(PARSESRC:%.c=%.o)
SEMANTOBJ = $(SEMANTSRC:%.c=%.o)
TARGET = parsetest semanttest

all: Makefile.obj $(TARGET)

Makefile.obj: $(ALLSRC)
	$(CC) $(MMFLAG) $^ > $@

parsetest: $(PARSEOBJ)
	$(LINK) $(LINKFLAG) $@ $^

semanttest: $(SEMANTOBJ)
	$(LINK) $(LINKFLAG) $@ $^

include Makefile.obj

%.o: %.c
	$(CC) $(CFLAG) $@ $<

y.tab.c: tiger.grm
	yacc -dv $<

lex.yy.c: tiger.lex
	lex $<

clean:
	-$(RM) *.o

distclean: clean
	-$(RM) $(TARGET)

rebuild: distclean all

.PHONY: clean all rebuild distclean

