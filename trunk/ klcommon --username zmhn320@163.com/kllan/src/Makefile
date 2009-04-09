# makefile for kl script core
CC = gcc
LINK = gcc
AR = ar
override CFLAG += -c -Wall -o
LINKFLAG = -o
MMFLAG = -MM
ARFLAG = crv
RM = rm 
SRCDIR = ./
LIBDIR = ../lib
TARGET = $(LIBDIR)/libkl.a
SRCLIST = $(wildcard *.c)
OBJLIST = $(SRCLIST:%.c=%.o)

ifeq ($(OS), Windows)
	RM = del
	LIBDIR = ..\lib
    TARGET = $(LIBDIR)\libkl.a	
else
	RM = rm
endif

all: Makefile.obj $(TARGET)

Makefile.obj: $(SRCLIST)
	$(CC) $(MMFLAG) $(SRCLIST) > $@

include Makefile.obj

%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAG) $@ $<

$(TARGET): $(OBJLIST)
	$(AR) $(ARFLAG) $@ $^ 

clean:
	-$(RM) *.o

distclean: clean
	-$(RM) $(TARGET)

rebuild: distclean all

.PHONY: clean all rebuild distclean

