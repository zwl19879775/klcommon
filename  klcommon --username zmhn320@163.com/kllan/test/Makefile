# makefile for kl script test, including the standalone interpreter
CC = gcc
LINK = gcc
override CFLAG += -c -Wall -I../src -I../plugin -o
MMFLAG = -MM -I../src -I../plugin
LINKFLAG = -o
RM = rm 
SRCDIR = ./
BINDIR = ../bin
LIBDIR = ../lib
LIB = $(LIBDIR)/libkl.a $(LIBDIR)/libklplugin.a
SRCLIST = $(wildcard *.c)
OBJLIST = $(SRCLIST:%.c=%.o)
ifeq ($(OS), Windows)
	BINDIR = ..\bin
	TESTLEX = $(BINDIR)\testlex.exe
	TESTSYNTAX = $(BINDIR)\testsyntax.exe
	TESTINTER = $(BINDIR)\testinter.exe
	TESTPLUGIN = $(BINDIR)\testplugin.exe
	KLINTERPRETER = $(BINDIR)\kl.exe
	RM = del
else
	TESTLEX = $(BINDIR)/testlex
	TESTSYNTAX = $(BINDIR)/testsyntax
	TESTINTER = $(BINDIR)/testinter
	TESTPLUGIN = $(BINDIR)/testplugin
	KLINTERPRETER = $(BINDIR)/kl
	RM = rm
endif

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

distclean: clean
	-$(RM) $(TESTLEX)
	-$(RM) $(TESTSYNTAX)
	-$(RM) $(TESTINTER)
	-$(RM) $(TESTPLUGIN)
	-$(RM) $(KLINTERPRETER)	

rebuild: distclean all

.PHONY: clean all rebuild distclean


