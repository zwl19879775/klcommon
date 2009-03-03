CC = gcc
LINK = gcc
CFLAG = -c -Wall -g -I../src -I../plugin -o
MMFLAG = -MM -I../src -I../plugin
LINKFLAG = -o
RM = del
SRCDIR = ./
TMPDIR = ../tmp
BINDIR = ../bin
LIBDIR = ../lib
LIB = $(LIBDIR)/libkl.a $(LIBDIR)/libklplugin.a
SRCLIST = $(wildcard *.c)
OBJLIST = $(SRCLIST:%.c=%.o)
EXTENSION = .exe
TESTLEX = $(BINDIR)/testlex$(EXTENSION)
TESTSYNTAX = $(BINDIR)/testsyntax$(EXTENSION)
TESTINTER = $(BINDIR)/testinter$(EXTENSION)
TESTPLUGIN = $(BINDIR)/testplugin$(EXTENSION)
KLINTERPRETER = $(BINDIR)/kl$(EXTENSION)

all: Makefile.obj $(TESTLEX) $(TESTSYNTAX) $(TESTINTER) $(TESTPLUGIN) $(KLINTERPRETER)

Makefile.obj: $(SRCLIST)
	$(CC) $(MMFLAG) $(SRCLIST) > $@

include Makefile.obj

%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAG) $@ $<

$(TESTLEX): testlex.o $(LIB)
	$(LINK) $(LINKFLAG) $@ $^

$(TESTSYNTAX): testsyntax.o $(LIB)
	$(LINK) $(LINKFLAG) $@ $^

$(TESTINTER): testinter.o $(LIB)
	$(LINK) $(LINKFLAG) $@ $^

$(TESTPLUGIN): testplugin.o $(LIB)
	$(LINK) $(LINKFLAG) $@ $^

$(KLINTERPRETER): kl.o $(LIB)
	$(LINK) $(LINKFLAG) $@ $^

clean:
	-$(RM) *.o

rebuild: clean all

.PHONY: clean all rebuild


